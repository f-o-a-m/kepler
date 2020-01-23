module Nameservice.Test.E2ESpec (spec) where

import           Control.Lens                         ((^.))
import           Data.Default.Class                   (def)
import           Data.Proxy
import           Nameservice.Modules.Nameservice      (BuyName (..),
                                                       DeleteName (..),
                                                       FaucetAccount (..),
                                                       Faucetted (..),
                                                       Name (..),
                                                       NameClaimed (..),
                                                       NameDeleted (..),
                                                       NameRemapped (..),
                                                       SetName (..), Whois (..))
import qualified Nameservice.Modules.Nameservice      as N (Api)
import           Nameservice.Test.EventOrphans        ()
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Servant.API                          ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Query         (QueryArgs (..),
                                                       defaultQueryWithData)
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Tendermint.SDK.Modules.Auth          (Amount (..), Coin (..),
                                                       CoinId (..))
import           Tendermint.SDK.Modules.Bank          (Transfer (..),
                                                       TransferEvent (..))
import qualified Tendermint.SDK.Modules.Bank          as Bank (Api)
import           Tendermint.SDK.Modules.TypedMessage  (TypedMessage (..))
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

  beforeAll (do faucetAccountAndCheckBalance user1 faucetAmount;
                faucetAccountAndCheckBalance user2 faucetAmount) $
    describe "Nameservice Spec" $ do
      it "Can query /health to make sure the node is alive" $ do
        resp <- runRPC RPC.health
        resp `shouldBe` RPC.ResultHealth

      it "Can create a name (success 0)" $ do
        let val = "hello world"
            msg = TypedMessage "BuyName" (encode $ BuyName 0 satoshi val addr1)
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
            msg = TypedMessage "SetName" (encode $ SetName satoshi addr1 newVal)
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
        let msg = TypedMessage "SetName" (encode $ SetName satoshi addr2 "goodbye to a world")
        ensureCheckAndDeliverResponseCodes (0,2) =<< mkSignedRawTransactionWithRoute "nameservice" user2 msg

      it "Can buy an existing name (success 0)" $ do
        (Coin _ balance1) <- getQueryResponseSuccess $ getBalance addr1 "nameservice"
        (Coin _ balance2) <- getQueryResponseSuccess $ getBalance addr2 "nameservice"
        Whois{whoisPrice} <- getQueryResponseSuccess $ getWhois $ defaultQueryWithData satoshi
        let purchaseAmount = whoisPrice + 1
            newVal = "hello (again) world"
            msg = TypedMessage "BuyName" (encode $ BuyName purchaseAmount satoshi newVal addr2)
            claimedLog = NameClaimed addr2 satoshi newVal purchaseAmount
        deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameClaimed" claimedLog
        -- check for updated balances - seller: addr1, buyer: addr2
        (Coin _ sellerFoundAmount) <- getQueryResponseSuccess $ getBalance addr1 "nameservice"
        sellerFoundAmount `shouldBe` (balance1 + purchaseAmount)
        (Coin _ buyerFoundAmount) <- getQueryResponseSuccess $ getBalance addr2 "nameservice"
        buyerFoundAmount `shouldBe` (balance2 - purchaseAmount)
        -- check for ownership changes
        let queryReq = defaultQueryWithData satoshi
        foundWhois <- getQueryResponseSuccess $ getWhois queryReq
        foundWhois `shouldBe` Whois "hello (again) world" addr2 purchaseAmount

      -- @NOTE: this is possibly a problem with the go application too
      -- https://cosmos.network/docs/tutorial/buy-name.html#msg
      it "Can buy self-owned names and make a profit (success 0)" $ do
        -- check balance before
        (Coin _ beforeBuyAmount) <- getQueryResponseSuccess $ getBalance addr1 "nameservice"
        -- buy
        let val = "hello (again) world"
            msg = TypedMessage "BuyName" (encode $ BuyName 500 satoshi val addr2)
            claimedLog = NameClaimed addr2 satoshi val 500
        deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user2 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameClaimed" claimedLog
        -- check balance after
        (Coin _ afterBuyAmount) <- getQueryResponseSuccess $ getBalance addr1 "nameservice"
        -- owner/buyer still profits
        putStrLn $ "AFTER BUY: " ++ show afterBuyAmount
        putStrLn $ "BEFORE BUY: " ++ show beforeBuyAmount
        afterBuyAmount `shouldSatisfy` (> beforeBuyAmount)

      it "Can fail to buy a name (failure 1)" $ do
        -- try to buy at a lower price
        let msg = TypedMessage "BuyName" (encode $ BuyName 0 satoshi "hello (again) world" addr1)
        mkSignedRawTransactionWithRoute "nameservice" user1 msg >>= ensureCheckAndDeliverResponseCodes (0,1)

      it "Can delete names (success 0)" $ do
        let msg = TypedMessage "DeleteName" (encode $ DeleteName addr2 satoshi)
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
        (Coin _ addr2Balance) <- getQueryResponseSuccess $ getBalance addr2 "nameservice"
        let tooMuchToTransfer = addr2Balance + 1
            msg = TypedMessage "Transfer" (encode $ Transfer addr2 addr1 "nameservice" tooMuchToTransfer)
        ensureCheckAndDeliverResponseCodes (0,1) =<< mkSignedRawTransactionWithRoute "bank" user2 msg

      it "Can transfer (success 0)" $ do
        (Coin _ balance1) <- getQueryResponseSuccess $ getBalance addr1 "nameservice"
        (Coin _ balance2) <- getQueryResponseSuccess $ getBalance addr2 "nameservice"
        let transferAmount = 1
            msg = TypedMessage "Transfer" $ encode
              Transfer
                { transferFrom = addr1
                , transferTo = addr2
                , transferCoinId = "nameservice"
                , transferAmount = transferAmount
                }
            transferEvent = TransferEvent
              { transferEventAmount = transferAmount
              , transferEventCoinId = "nameservice"
              , transferEventTo = addr2
              , transferEventFrom = addr1
              }
        deliverResp <- mkSignedRawTransactionWithRoute "bank" user1 msg >>= getDeliverTxResponse
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "TransferEvent" transferEvent
        -- check balances
        (Coin _ balance1') <- getQueryResponseSuccess $ getBalance addr1 "nameservice"
        balance1' `shouldBe` balance1 - transferAmount
        (Coin _ balance2') <- getQueryResponseSuccess $ getBalance addr2 "nameservice"
        balance2' `shouldBe` balance2 + transferAmount

--------------------------------------------------------------------------------

user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: User
user2 = makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

--------------------------------------------------------------------------------

faucetAccountAndCheckBalance :: User -> Amount -> IO ()
faucetAccountAndCheckBalance user@User{userAddress} amount = do
  (Coin _ balBefore) <- getQueryResponseSuccess $ getBalance userAddress "nameservice"
  let msg = TypedMessage "FaucetAccount" (encode $ FaucetAccount userAddress "nameservice" amount)
      faucetEvent = Faucetted userAddress "nameservice" amount
  deliverResp <- mkSignedRawTransactionWithRoute "nameservice" user msg >>= getDeliverTxResponse
  ensureDeliverResponseCode deliverResp 0
  ensureEventLogged deliverResp "Faucetted" faucetEvent
  (Coin _ balAfter) <- getQueryResponseSuccess $ getBalance userAddress "nameservice"
  (balAfter-balBefore) `shouldBe` amount

getBalance :: Address -> CoinId -> RPC.TendermintM (ClientResponse Coin)
getWhois :: QueryArgs Name -> RPC.TendermintM (ClientResponse Whois)

apiP :: Proxy ("bank" :> Bank.Api :<|> ("nameservice" :> N.Api))
apiP = Proxy

(getBalance :<|> getWhois) =
  genClient (Proxy :: Proxy RPC.TendermintM) apiP def
