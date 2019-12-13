module Nameservice.Test.E2ESpec where

import           Control.Lens                           ((^.))
import           Crypto.Secp256k1                       (CompactRecSig (..),
                                                         SecKey, derivePubKey,
                                                         exportCompactRecSig,
                                                         secKey)
import           Data.Aeson                             (ToJSON)
import           Data.Aeson.Encode.Pretty               (encodePretty)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteArray.HexString               as Hex
import           Data.ByteString                        (ByteString, snoc)
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BL
import           Data.ByteString.Short                  (fromShort)
import           Data.Default.Class                     (def)
import           Data.Either                            (partitionEithers)
import           Data.Maybe                             (fromJust)
import           Data.Proxy
import           Data.String                            (fromString)
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           Data.Word                              (Word32)
import           Nameservice.Modules.Nameservice        (BuyName (..),
                                                         DeleteName (..),
                                                         Name (..),
                                                         NameClaimed (..),
                                                         NameDeleted (..),
                                                         NameRemapped (..),
                                                         SetName (..),
                                                         Whois (..))
import qualified Nameservice.Modules.Nameservice        as N (Api)
import           Nameservice.Modules.Token              (Amount (..),
                                                         FaucetAccount (..),
                                                         Faucetted (..),
                                                         Transfer (..),
                                                         TransferEvent (..))
import qualified Nameservice.Modules.Token              as T (Api)
import           Nameservice.Modules.TypedMessage       (TypedMessage (..))
import           Network.ABCI.Types.Messages.FieldTypes (Event (..))
import qualified Network.ABCI.Types.Messages.Response   as Response
import qualified Network.Tendermint.Client              as RPC
import           Proto3.Suite                           (Message,
                                                         toLazyByteString)
import           Servant.API                            ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp                 (QueryApi)
import           Tendermint.SDK.BaseApp.Query           (QueryArgs (..),
                                                         defaultQueryWithData)
import           Tendermint.SDK.Codec                   (HasCodec (..))
import           Tendermint.SDK.Crypto                  (Secp256k1,
                                                         addressFromPubKey)
import           Tendermint.SDK.Types.Address           (Address (..))
import           Tendermint.SDK.Types.Transaction       (RawTransaction (..),
                                                         signRawTransaction)
import           Tendermint.Utils.Client                (ClientResponse (..),
                                                         HasClient (..),
                                                         RunClient (..))
import           Tendermint.Utils.Events                (FromEvent (..))
import           Tendermint.Utils.Response              (ensureDeliverResponseCode,
                                                         ensureEventLogged)
import           Tendermint.Utils.User                  (User (..), makeUser, mkSignedRawTransactionWithRoute)
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
        let oldVal = "goodbye to a world"
            newVal = "hello (again) world"
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

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")

faucetAccount :: User -> IO ()
faucetAccount User{userAddress, userPrivKey} = do
  let msg = TypedMessage "FaucetAccount" (encode $ FaucetAccount userAddress 1000)
      faucetEvent = Faucetted userAddress 1000
      rawTx = mkSignedRawTransactionWithRoute "token" userPrivKey msg
  deliverResp <- getDeliverTxResponse rawTx
  ensureDeliverResponseCode deliverResp 0
  ensureEventLogged deliverResp "Faucetted" faucetEvent

-- executes a query and ensures a 0 response code
getQueryResponseSuccess :: RPC.TendermintM (ClientResponse a) -> IO a
getQueryResponseSuccess query = do
  ClientResponse{clientResponseData,clientResponseRaw} <- runRPC query
  let responseCode = clientResponseRaw ^. Response._queryCode
  responseCode `shouldBe` 0
  return . fromJust $ clientResponseData

-- executes a request, then returns the checkTx response
getCheckTxResponse :: RawTransaction -> IO Response.CheckTx
getCheckTxResponse rawTx = do
  let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  fmap RPC.resultBroadcastTxCommitCheckTx . runRPC $
    RPC.broadcastTxCommit txReq

-- executes a request, then returns the deliverTx response
getDeliverTxResponse :: RawTransaction -> IO Response.DeliverTx
getDeliverTxResponse rawTx = do
  let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $
    RPC.broadcastTxCommit txReq

-- executes a request, check deliver and response codes
ensureCheckAndDeliverResponseCodes :: (Word32, Word32) -> RawTransaction -> IO ()
ensureCheckAndDeliverResponseCodes codes rawTx = do
  let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  resp <- runRPC $ RPC.broadcastTxCommit txReq
  let checkResp = RPC.resultBroadcastTxCommitCheckTx resp
      deliverResp = RPC.resultBroadcastTxCommitDeliverTx resp
  codes `shouldBe` (checkResp ^. Response._checkTxCode, deliverResp ^. Response._deliverTxCode)

--------------------------------------------------------------------------------

encodeRawTx :: RawTransaction -> Base64.Base64String
encodeRawTx = Base64.fromBytes . encode

user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: User
user2 = makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

--------------------------------------------------------------------------------

getWhois :: QueryArgs Name -> RPC.TendermintM (ClientResponse Whois)
getBalance :: QueryArgs Address -> RPC.TendermintM (ClientResponse Amount)

apiP :: Proxy ("token" :> T.Api :<|> ("nameservice" :> N.Api))
apiP = Proxy

(getBalance :<|> getWhois) =
  genClient (Proxy :: Proxy RPC.TendermintM) apiP def
