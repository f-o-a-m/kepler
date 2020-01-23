module Nameservice.Test.E2ESpec (spec) where

import           Control.Lens                         ((^.))
import           Control.Monad                        (void)
import           Data.Default.Class                   (def)
import           Data.Proxy
import           Nameservice.Modules.Nameservice      (BuyName (..),
                                                       DeleteName (..),
                                                       Name (..),
                                                       NameClaimed (..),
                                                       NameDeleted (..),
                                                       NameRemapped (..),
                                                       SetName (..), Whois (..))
import qualified Nameservice.Modules.Nameservice      as N (QueryApi)
import           Nameservice.Modules.Token            (Amount (..),
                                                       FaucetAccount (..),
                                                       Faucetted (..),
                                                       Transfer (..),
                                                       TransferEvent (..))
import qualified Nameservice.Modules.Token            as T (QueryApi)
import           Nameservice.Test.EventOrphans        ()
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Servant.API                          ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Query         (QueryArgs (..),
                                                       defaultQueryWithData)
import           Tendermint.SDK.Types.Address         (Address (..))
import           Tendermint.Utils.Client              (ClientResponse (..),
                                                       HasClient (..))
import           Tendermint.Utils.Request             (ensureCheckAndDeliverResponseCodes,
                                                       getDeliverTxResponse,
                                                       getQueryResponseSuccess,
                                                       runRPC)
import           Tendermint.Utils.Response            (ensureDeliverResponseCode,
                                                       ensureEventLogged)
import           Tendermint.Utils.User                (User (..), makeUser, mkSignedRawTransactionWithRoute)
import           Test.Hspec

spec :: Spec
spec = do
  let satoshi = Name "satoshi"
      addr1 = userAddress user1
      addr2 = userAddress user2
      faucetAmount = 1000

  beforeAll (do faucetAccount user1 faucetAmount; faucetAccount user2 faucetAmount) $
    describe "Nameservice Spec" $ do
      it "Can query /health to make sure the node is alive" $ do
        resp <- runRPC RPC.health
        resp `shouldBe` RPC.ResultHealth

      it "Can query account balances" $ do
        let queryReq = defaultQueryWithData addr1
        void $ getQueryResponseSuccess $ getBalance queryReq

      it "Can create a name (success 0)" $ do
        let val = "hello world"
            msg = BuyName 0 satoshi val addr1
            claimedLog = NameClaimed addr1 satoshi val 0
        deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user1 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameClaimed" claimedLog

      it "Can query for a name" $ do
        let queryReq = defaultQueryWithData satoshi
        foundWhois <- getQueryResponseSuccess $ getWhois queryReq
        foundWhois `shouldBe` Whois "hello world" addr1 0

      it "Can query for a name that doesn't exist" $ do
        let nope = Name "nope"
            queryReq = defaultQueryWithData nope
        ClientResponse{ clientResponseData, clientResponseRaw } <- runRPC $ getWhois queryReq
        let queryRespCode = clientResponseRaw ^. Response._queryCode
        -- storage failure
        queryRespCode `shouldBe` 2
        clientResponseData `shouldBe` Nothing

      it "Can set a name value (success 0)" $ do
        let oldVal = "hello world"
            newVal = "goodbye to a world"
            msg = SetName satoshi addr1 newVal
            remappedLog = NameRemapped satoshi oldVal newVal
        deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user1 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameRemapped" remappedLog
        -- check for changes
        let queryReq = defaultQueryWithData satoshi
        foundWhois <- getQueryResponseSuccess $ getWhois queryReq
        foundWhois `shouldBe` Whois "goodbye to a world" addr1 0

      it "Can fail to set a name (failure 2)" $ do
        -- try to set a name without being the owner
        let msg =  SetName satoshi addr2 "goodbye to a world"
        ensureCheckAndDeliverResponseCodes (0,2) =<< mkSignedRawTransactionWithRoute "nameservice" user2 msg

      it "Can buy an existing name (success 0)" $ do
        balance1 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr1
        balance2 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr2
        Whois{whoisPrice} <- getQueryResponseSuccess $ getWhois $ defaultQueryWithData satoshi
        let purchaseAmount = whoisPrice + 1
            newVal = "hello (again) world"
            msg = BuyName purchaseAmount satoshi newVal addr2
            claimedLog = NameClaimed addr2 satoshi newVal purchaseAmount
        deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameClaimed" claimedLog
        -- check for updated balances - seller: addr1, buyer: addr2
        let sellerQueryReq = defaultQueryWithData addr1
        sellerFoundAmount <- getQueryResponseSuccess $ getBalance sellerQueryReq
        sellerFoundAmount `shouldBe` (balance1 + purchaseAmount)
        let buyerQueryReq = defaultQueryWithData addr2
        buyerFoundAmount <- getQueryResponseSuccess $ getBalance buyerQueryReq
        buyerFoundAmount `shouldBe` (balance2 - purchaseAmount)
        -- check for ownership changes
        let queryReq = defaultQueryWithData satoshi
        foundWhois <- getQueryResponseSuccess $ getWhois queryReq
        foundWhois `shouldBe` Whois "hello (again) world" addr2 purchaseAmount

      -- @NOTE: this is possibly a problem with the go application too
      -- https://cosmos.network/docs/tutorial/buy-name.html#msg
      it "Can buy self-owned names and make a profit (success 0)" $ do
        -- check balance before
        let queryReq = defaultQueryWithData addr2
        beforeBuyAmount <- getQueryResponseSuccess $ getBalance queryReq
        -- buy
        let val = "hello (again) world"
            msg = BuyName 500 satoshi val addr2
            claimedLog = NameClaimed addr2 satoshi val 500
        deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameClaimed" claimedLog
        -- check balance after
        afterBuyAmount <- getQueryResponseSuccess $ getBalance queryReq
        -- owner/buyer still profits
        afterBuyAmount `shouldSatisfy` (> beforeBuyAmount)

      it "Can fail to buy a name (failure 1)" $ do
        -- try to buy at a lower price
        let msg = BuyName 100 satoshi "hello (again) world" addr1
        mkSignedRawTransactionWithRoute "nameservice" user1 msg >>= ensureCheckAndDeliverResponseCodes (0,1)

      it "Can delete names (success 0)" $ do
        let msg = DeleteName addr2 satoshi
            deletedLog = NameDeleted satoshi
        deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameDeleted" deletedLog
        -- name shouldn't exist
        let queryReq = defaultQueryWithData satoshi
        ClientResponse{ clientResponseData, clientResponseRaw } <- runRPC $ getWhois queryReq
        let queryRespCode = clientResponseRaw ^. Response._queryCode
        -- storage failure
        queryRespCode `shouldBe` 2
        clientResponseData `shouldBe` Nothing

      it "Can fail a transfer (failure 1)" $ do
        let senderBeforeQueryReq = defaultQueryWithData addr2
        addr2Balance <- getQueryResponseSuccess $ getBalance senderBeforeQueryReq
        let tooMuchToTransfer = addr2Balance + 1
            msg = Transfer addr2 addr1 tooMuchToTransfer
        ensureCheckAndDeliverResponseCodes (0,1) =<< mkSignedRawTransactionWithRoute "token" user2 msg

      it "Can transfer (success 0)" $ do
        balance1 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr1
        balance2 <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr2
        let transferAmount = 1
            msg =
              Transfer
                { transferFrom = addr1
                , transferTo = addr2
                , transferAmount = transferAmount
                }
            transferEvent = TransferEvent
              { transferEventAmount = transferAmount
              , transferEventTo = addr2
              , transferEventFrom = addr1
              }
        deliverResp <- mkSignedRawTransactionWithRoute "token" user1 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "TransferEvent" transferEvent
        -- check balances
        balance1' <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr1
        balance1' `shouldBe` balance1 - transferAmount
        balance2' <- getQueryResponseSuccess $ getBalance $ defaultQueryWithData addr2
        balance2' `shouldBe` balance2 + transferAmount

--------------------------------------------------------------------------------

user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: User
user2 = makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

--------------------------------------------------------------------------------

faucetAccount :: User -> Amount -> IO ()
faucetAccount user@User{userAddress} amount = do
  let msg = FaucetAccount userAddress amount
      faucetEvent = Faucetted userAddress amount
  deliverResp <- mkSignedRawTransactionWithRoute "token" user msg >>= getDeliverTxResponse
  ensureDeliverResponseCode deliverResp 0
  ensureEventLogged deliverResp "Faucetted" faucetEvent

getWhois :: QueryArgs Name -> RPC.TendermintM (ClientResponse Whois)
getBalance :: QueryArgs Address -> RPC.TendermintM (ClientResponse Amount)

apiP :: Proxy ("token" :> T.QueryApi :<|> ("nameservice" :> N.QueryApi))
apiP = Proxy

(getBalance :<|> getWhois) =
  genClient (Proxy :: Proxy RPC.TendermintM) apiP def
