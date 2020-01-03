module Nameservice.Test.E2ESpec (spec) where

import           Control.Lens                         ((^.))
import           Data.Default.Class                   (def)
import           Data.Proxy
import           Nameservice.Modules.Nameservice      (BuyName (..),
                                                       DeleteName (..),
                                                       Name (..),
                                                       NameClaimed (..),
                                                       NameDeleted (..),
                                                       NameRemapped (..),
                                                       SetName (..), Whois (..))
import qualified Nameservice.Modules.Nameservice      as N (Api)
import           Nameservice.Modules.Token            (Amount (..),
                                                       FaucetAccount (..),
                                                       Faucetted (..),
                                                       Transfer (..),
                                                       TransferEvent (..))
import qualified Nameservice.Modules.Token            as T (Api)
import           Nameservice.Modules.TypedMessage     (TypedMessage (..))
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Servant.API                          ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Query         (QueryArgs (..),
                                                       defaultQueryWithData)
import           Tendermint.SDK.Codec                 (HasCodec (..))
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
      privateKey1 = userPrivKey user1
      addr2 = userAddress user2
      privateKey2 = userPrivKey user2

  beforeAll (do faucetAccount user1; faucetAccount user2) $
    describe "Nameservice Spec" $ do
      it "Can query /health to make sure the node is alive" $ do
        resp <- runRPC RPC.health
        resp `shouldBe` RPC.ResultHealth

      it "Can query account balances" $ do
        let queryReq = defaultQueryWithData addr1
        foundAmount <- getQueryResponseSuccess $ getBalance queryReq
        foundAmount `shouldBe` Amount 1000

      it "Can create a name (success 0)" $ do
        let val = "hello world"
            msg = TypedMessage "BuyName" (encode $ BuyName 0 satoshi val addr1)
            claimedLog = NameClaimed addr1 satoshi val 0
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
        deliverResp <- getDeliverTxResponse rawTx
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
        queryRespCode `shouldBe` 1
        clientResponseData `shouldBe` Nothing

      it "Can set a name value (success 0)" $ do
        let oldVal = "hello world"
            newVal = "goodbye to a world"
            msg = TypedMessage "SetName" (encode $ SetName satoshi addr1 newVal)
            remappedLog = NameRemapped satoshi oldVal newVal
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameRemapped" remappedLog
        -- check for changes
        let queryReq = defaultQueryWithData satoshi
        foundWhois <- getQueryResponseSuccess $ getWhois queryReq
        foundWhois `shouldBe` Whois "goodbye to a world" addr1 0

      it "Can fail to set a name (failure 2)" $ do
        -- try to set a name without being the owner
        let msg = TypedMessage "SetName" (encode $ SetName satoshi addr2 "goodbye to a world")
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        ensureCheckAndDeliverResponseCodes (0,2) rawTx

      it "Can buy an existing name (success 0)" $ do
        let newVal = "hello (again) world"
            msg = TypedMessage "BuyName" (encode $ BuyName 300 satoshi newVal addr2)
            claimedLog = NameClaimed addr2 satoshi newVal 300
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameClaimed" claimedLog
        -- check for updated balances - seller: addr1, buyer: addr2
        let sellerQueryReq = defaultQueryWithData addr1
        sellerFoundAmount <- getQueryResponseSuccess $ getBalance sellerQueryReq
        sellerFoundAmount `shouldBe` Amount 1300
        let buyerQueryReq = defaultQueryWithData addr2
        buyerFoundAmount <- getQueryResponseSuccess $ getBalance buyerQueryReq
        buyerFoundAmount `shouldBe` Amount 700
        -- check for ownership changes
        let queryReq = defaultQueryWithData satoshi
        foundWhois <- getQueryResponseSuccess $ getWhois queryReq
        foundWhois `shouldBe` Whois "hello (again) world" addr2 300

      -- @NOTE: this is possibly a problem with the go application too
      -- https://cosmos.network/docs/tutorial/buy-name.html#msg
      it "Can buy self-owned names and make a profit (success 0)" $ do
        -- check balance before
        let queryReq = defaultQueryWithData addr2
        beforeBuyAmount <- getQueryResponseSuccess $ getBalance queryReq
        -- buy
        let val = "hello (again) world"
            msg = TypedMessage "BuyName" (encode $ BuyName 500 satoshi val addr2)
            claimedLog = NameClaimed addr2 satoshi val 500
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameClaimed" claimedLog
        -- check balance after
        afterBuyAmount <- getQueryResponseSuccess $ getBalance queryReq
        -- owner/buyer still profits
        afterBuyAmount `shouldSatisfy` (> beforeBuyAmount)

      it "Can fail to buy a name (failure 1)" $ do
        -- try to buy at a lower price
        let msg = TypedMessage "BuyName" (encode $ BuyName 100 satoshi "hello (again) world" addr1)
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
        ensureCheckAndDeliverResponseCodes (0,1) rawTx

      it "Can delete names (success 0)" $ do
        let msg = TypedMessage "DeleteName" (encode $ DeleteName addr2 satoshi)
            deletedLog = NameDeleted satoshi
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "NameDeleted" deletedLog
        -- name shouldn't exist
        let queryReq = defaultQueryWithData satoshi
        ClientResponse{ clientResponseData, clientResponseRaw } <- runRPC $ getWhois queryReq
        let queryRespCode = clientResponseRaw ^. Response._queryCode
        -- storage failure
        queryRespCode `shouldBe` 1
        clientResponseData `shouldBe` Nothing

      it "Can fail a transfer (failure 1)" $ do
        let msg = TypedMessage "Transfer" (encode $ Transfer addr2 addr1 2000)
            rawTx = mkSignedRawTransactionWithRoute "token" privateKey1 msg
        ensureCheckAndDeliverResponseCodes (0,1) rawTx

      it "Can transfer (success 0)" $ do
        let senderBeforeQueryReq = defaultQueryWithData addr2
        senderBeforeFoundAmount <- getQueryResponseSuccess $ getBalance senderBeforeQueryReq
        senderBeforeFoundAmount `shouldBe` Amount 1700
        let msg = TypedMessage "Transfer" (encode $ Transfer addr1 addr2 500)
            transferEvent = TransferEvent 500 addr1 addr2
            rawTx = mkSignedRawTransactionWithRoute "token" privateKey1 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        ensureEventLogged deliverResp "TransferEvent" transferEvent
        -- check balances
        let receiverQueryReq = defaultQueryWithData addr1
        receiverFoundAmount <- getQueryResponseSuccess $ getBalance receiverQueryReq
        receiverFoundAmount `shouldBe` Amount 1800
        let senderAfterQueryReq = defaultQueryWithData addr2
        senderAfterFoundAmount <- getQueryResponseSuccess $ getBalance senderAfterQueryReq
        senderAfterFoundAmount `shouldBe` Amount 1200

--------------------------------------------------------------------------------
user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: User
user2 = makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

--------------------------------------------------------------------------------

faucetAccount :: User -> IO ()
faucetAccount User{userAddress, userPrivKey} = do
  let msg = TypedMessage "FaucetAccount" (encode $ FaucetAccount userAddress 1000)
      faucetEvent = Faucetted userAddress 1000
      rawTx = mkSignedRawTransactionWithRoute "token" userPrivKey msg
  deliverResp <- getDeliverTxResponse rawTx
  ensureDeliverResponseCode deliverResp 0
  ensureEventLogged deliverResp "Faucetted" faucetEvent

getWhois :: QueryArgs Name -> RPC.TendermintM (ClientResponse Whois)
getBalance :: QueryArgs Address -> RPC.TendermintM (ClientResponse Amount)

apiP :: Proxy ("token" :> T.Api :<|> ("nameservice" :> N.Api))
apiP = Proxy

(getBalance :<|> getWhois) =
  genClient (Proxy :: Proxy RPC.TendermintM) apiP def
