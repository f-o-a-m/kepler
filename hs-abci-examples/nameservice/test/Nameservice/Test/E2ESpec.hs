module Nameservice.Test.E2ESpec where

import           Control.Lens                           ((^.))
import           Crypto.Secp256k1                       (SecKey, derivePubKey,
                                                         exportCompactRecSig,
                                                         secKey)
import           Data.Aeson                             (ToJSON)
import           Data.Aeson.Encode.Pretty               (encodePretty)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteArray.HexString               as Hex
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Lazy                   as BL
import           Data.Default.Class                     (def)
import           Data.Either                            (partitionEithers)
import           Data.Maybe                             (fromJust)
import           Data.Proxy
import qualified Data.Serialize                         as Serialize
import           Data.String                            (fromString)
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           Data.Word                              (Word32)
import           Nameservice.Application                (QueryApi)
import           Nameservice.Modules.Nameservice        (BuyName (..),
                                                         DeleteName (..),
                                                         FaucetAccount (..),
                                                         Name (..),
                                                         NameClaimed (..),
                                                         NameDeleted (..),
                                                         NameRemapped (..),
                                                         SetName (..),
                                                         Whois (..))
import           Nameservice.Modules.Token              (Amount (..),
                                                         Transfer (..))
import           Network.ABCI.Types.Messages.FieldTypes (Event (..))
import qualified Network.ABCI.Types.Messages.Response   as Response
import qualified Network.Tendermint.Client              as RPC
import           Proto3.Suite                           (Message,
                                                         toLazyByteString)
import           Servant.API                            ((:<|>) (..))
import           Tendermint.SDK.Codec                   (HasCodec (..))
import           Tendermint.SDK.Crypto                  (Secp256k1,
                                                         addressFromPubKey)
import           Tendermint.SDK.Events                  (FromEvent (..))
import           Tendermint.SDK.Query.Client            (ClientResponse (..),
                                                         genClient)
import           Tendermint.SDK.Query.Types             (QueryArgs (..))
import           Tendermint.SDK.Types.Address           (Address (..))
import           Tendermint.SDK.Types.Transaction       (RawTransaction (..),
                                                         signRawTransaction)
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
        let queryReq = defaultReqWithData addr1
        ClientResponse{clientResponseData = foundAmount} <- runRPC $ getBalance queryReq
        foundAmount `shouldBe` Amount 1000

      it "Can create a name" $ do
        let val = "hello world"
            msg = BuyName 0 satoshi val addr1
            claimedLog = NameClaimed addr1 satoshi val 0
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        (errs, events) <- deliverTxEvents deliverResp "NameClaimed"
        errs `shouldBe` mempty
        events `shouldSatisfy` elem claimedLog

      it "Can query for a name" $ do
        let queryReq = defaultReqWithData satoshi
        ClientResponse{clientResponseData = foundWhois} <- runRPC $ getWhois queryReq
        whoisValue foundWhois `shouldBe` "hello world"
        whoisOwner foundWhois `shouldBe` addr1
        whoisPrice foundWhois `shouldBe` 0

      it "Can query for a name that doesn't exist" $ do
        let nope = Name "nope"
            queryReq = defaultReqWithData nope
        ClientResponse{ clientResponseData = emptyWhois
                      , clientResponseRaw
                      } <- runRPC $ getWhois queryReq
        let queryRespCode = clientResponseRaw ^. Response._queryCode
        -- storage failure
        queryRespCode `shouldBe` 1
        -- empty whois (defaults)
        whoisPrice emptyWhois `shouldBe` 0
        whoisOwner emptyWhois `shouldBe` Address ""
        whoisValue emptyWhois `shouldBe` ""

      it "Can set a name value" $ do
        let oldVal = "hello world"
            newVal = "goodbye to a world"
            msg = SetName satoshi addr1 newVal
            remappedLog = NameRemapped satoshi oldVal newVal
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        (errs, events) <- deliverTxEvents deliverResp "NameRemapped"
        errs `shouldBe` mempty
        events `shouldSatisfy` elem remappedLog
        -- check for changes
        let queryReq = defaultReqWithData satoshi
        ClientResponse{clientResponseData = foundWhois} <- runRPC $ getWhois queryReq
        whoisValue foundWhois `shouldBe` "goodbye to a world"
        -- eveyrthing else should remain the same
        whoisOwner foundWhois `shouldBe` addr1
        whoisPrice foundWhois `shouldBe` 0

      it "Can fail to set a name" $ do
        -- try to set a name without being the owner
        let msg = SetName satoshi addr2 "goodbye to a world"
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 2

      it "Can buy an existing name" $ do
        -- how to do both types of logs?
        let oldVal = "goodbye to a world"
            newVal = "hello (again) world"
            msg = BuyName 300 satoshi newVal addr2
            claimedLog = NameClaimed addr2 satoshi newVal 300
            -- transferLog = Transfer 300 addr1 addr2
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        (errs, events) <- deliverTxEvents deliverResp "NameClaimed"
        errs `shouldBe` mempty
        events `shouldSatisfy` elem claimedLog
        -- events `shouldSatisfy` elem transferLog
        -- check for ownership changes
        let queryReq = defaultReqWithData satoshi
        ClientResponse{clientResponseData = foundWhois} <- runRPC $ getWhois queryReq
        whoisOwner foundWhois `shouldBe` addr2
        whoisPrice foundWhois `shouldBe` 300
        whoisValue foundWhois `shouldBe` "hello (again) world"

      -- @NOTE: this is possibly a problem with the go application too
      -- https://cosmos.network/docs/tutorial/buy-name.html#msg
      it "Can buy self-owned names (and make a profit)" $ do
        -- check balance before
        let queryReq = defaultReqWithData addr2
        ClientResponse{clientResponseData = beforeBuyAmount} <- runRPC $ getBalance queryReq
        -- buy
        let val = "hello (again) world"
            msg = BuyName 500 satoshi val addr2
            claimedLog = NameClaimed addr2 satoshi val 500
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        (errs, events) <- deliverTxEvents deliverResp "NameClaimed"
        errs `shouldBe` mempty
        events `shouldSatisfy` elem claimedLog
        -- check balance after
        ClientResponse{clientResponseData = afterBuyAmount} <- runRPC $ getBalance queryReq
        -- owner/buyer still profits
        beforeBuyAmount `shouldSatisfy` (< afterBuyAmount)

      it "Can fail to buy a name" $ do
        -- try to buy at a lower price
        let msg = BuyName 100 satoshi "hello (again) world" addr1
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 1

      it "Can delete names" $ do
        let msg = DeleteName addr2 satoshi
            deletedLog = NameDeleted satoshi
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
        deliverResp <- getDeliverTxResponse rawTx
        ensureDeliverResponseCode deliverResp 0
        (errs, events) <- deliverTxEvents deliverResp "NameDeleted"
        errs `shouldBe` mempty
        events `shouldSatisfy` elem deletedLog
        -- name shouldn't exist
        let queryReq = defaultReqWithData satoshi
        ClientResponse{ clientResponseData = emptyWhois
                      , clientResponseRaw
                      } <- runRPC $ getWhois queryReq
        let queryRespCode = clientResponseRaw ^. Response._queryCode
        -- storage failure
        queryRespCode `shouldBe` 1
        -- should be a default whois
        whoisPrice emptyWhois `shouldBe` 0
        whoisOwner emptyWhois `shouldBe` Address ""
        whoisValue emptyWhois `shouldBe` ""

      -- @TODO: make transfer messages
      it "Can fail a transfer" $ do
        -- try to give addr1 2000 from addr2
        pendingWith "Split token module"

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
  let msg = FaucetAccount userAddress 1000
      -- @NOTE: why is this `nameservice` and not `token`?
      rawTx = mkSignedRawTransactionWithRoute "nameservice" userPrivKey msg
      txReq =
        RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  _ <- runRPC $ RPC.broadcastTxCommit txReq
  return ()

-- executes a request, then returns the deliverTx response
getDeliverTxResponse :: RawTransaction -> IO Response.DeliverTx
getDeliverTxResponse rawTx = do
  let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $
    RPC.broadcastTxCommit txReq

-- get the logged events from a deliver response,
-- ensures there are no errors when parsing event logs
deliverTxEvents :: FromEvent e => Response.DeliverTx -> Text -> IO ([Text],[e])
deliverTxEvents deliverResp eventName = do
  let deliverEvents = deliverResp ^. Response._deliverTxEvents
      filtered = filter ((== eventName) . eventType) deliverEvents
  return . partitionEithers . map fromEvent $ filtered

-- check for a specific deliver response code
ensureDeliverResponseCode :: Response.DeliverTx -> Word32 -> IO ()
ensureDeliverResponseCode deliverResp code = do
  let deliverRespCode = deliverResp ^. Response._deliverTxCode
  deliverRespCode `shouldBe` code

-- this probably should be in a default type class
defaultReqWithData :: a -> QueryArgs a
defaultReqWithData x = QueryArgs
  { queryArgsData = x
  , queryArgsHeight = 0
  , queryArgsProve = False
  }

--------------------------------------------------------------------------------

decodeValue :: HasCodec a => Base64.Base64String -> a
decodeValue = (\(Right a) -> a) . decode . Base64.toBytes

encodeRawTx :: RawTransaction -> Base64.Base64String
encodeRawTx = Base64.fromBytes . encode

encodeMsgData :: Message a => a -> BS.ByteString
encodeMsgData = BL.toStrict . toLazyByteString

-- sign a trx with a user's private key
mkSignedRawTransactionWithRoute :: Message a => BS.ByteString -> SecKey -> a -> RawTransaction
mkSignedRawTransactionWithRoute route privateKey msg = sign unsigned
  where unsigned = RawTransaction { rawTransactionData = encodeMsgData msg
                                  , rawTransactionRoute = cs route
                                  , rawTransactionSignature = ""
                                  }
        sig = signRawTransaction algProxy privateKey unsigned
        sign rt = rt { rawTransactionSignature = Serialize.encode $ exportCompactRecSig sig }

data User = User
  { userPrivKey :: SecKey
  , userAddress :: Address
  }

user1 :: User
user1 = makeUser "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

user2 :: User
user2 = makeUser "f65242094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

makeUser :: String -> User
makeUser privKeyStr =
  let privateKey = fromJust . secKey . Hex.toBytes . fromString $ privKeyStr
      pubKey = derivePubKey privateKey
      address = addressFromPubKey (Proxy @Secp256k1) pubKey
  in User privateKey address

algProxy :: Proxy Secp256k1
algProxy = Proxy

--------------------------------------------------------------------------------

getWhois :: QueryArgs Name -> RPC.TendermintM (ClientResponse Whois)
getBalance :: QueryArgs Address -> RPC.TendermintM (ClientResponse Amount)

(getBalance :<|> getWhois) =
  genClient (Proxy :: Proxy RPC.TendermintM) (Proxy :: Proxy QueryApi) def
