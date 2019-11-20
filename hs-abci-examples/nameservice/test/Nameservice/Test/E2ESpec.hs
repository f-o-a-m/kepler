module Nameservice.Test.E2ESpec where

import           Control.Lens                         (to, (^.))
import           Crypto.Secp256k1                     (SecKey,
                                                       derivePubKey,
                                                       exportCompactRecSig,
                                                       secKey)
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteArray.HexString             as Hex
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Lazy                 as BL
import           Data.Default.Class                   (def)
import           Data.Maybe                           (fromJust)
import           Data.Proxy
import qualified Data.Serialize                       as Serialize
import           Data.String                          (fromString)
import           Data.String.Conversions              (cs)
import           Nameservice.Application              (QueryApi)
import           Nameservice.Modules.Nameservice      (BuyName (..),
                                                       DeleteName (..),
                                                       FaucetAccount (..),
                                                       Name (..),
                                                       NameserviceMessage (..),
                                                       SetName (..), Whois (..))
import           Nameservice.Modules.Token            (Amount (..))
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Proto3.Suite                         (Message,
                                                       toLazyByteString)
import           Servant.API                          ((:<|>) (..))
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Tendermint.SDK.Crypto                (Secp256k1,
                                                       addressFromPubKey)
import           Tendermint.SDK.Query.Client          (ClientResponse (..), genClient)
import           Tendermint.SDK.Query.Types           (QueryArgs (..))
import           Tendermint.SDK.Types.Address         (Address (..))
import           Tendermint.SDK.Types.Transaction     (RawTransaction (..),
                                                       signRawTransaction)
import           Test.Hspec

spec :: Spec
spec = do
  let satoshi = Name "satoshi"
      addr0 = userAddress user0
      privateKey0 = userPrivKey user0
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
        let queryReq = QueryArgs
              { queryArgsData = addr1
              , queryArgsHeight = 0
              , queryArgsProve = False
              }
        ClientResponse{clientResponseData = foundAmount} <- runRPC $ getBalance queryReq
        foundAmount `shouldBe` (Amount 1000)

      it "Can create a name" $ do
        let msg = BuyName 0 satoshi "hello world" addr1
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        deliverRespCode `shouldBe` 0

      it "Can query for a name" $ do
        let queryReq = QueryArgs
              { queryArgsData = satoshi
              , queryArgsHeight = 0
              , queryArgsProve = False
              }
        ClientResponse{clientResponseData = foundWhois} <- runRPC $ getWhois queryReq
        whoisValue foundWhois `shouldBe` "hello world"
        whoisOwner foundWhois `shouldBe` addr1
        whoisPrice foundWhois `shouldBe` 0

      it "Can query for a name that doesn't exist" $ do
        let nope = Name "nope"
            queryReq = QueryArgs
              { queryArgsData = nope
              , queryArgsHeight = 0
              , queryArgsProve = False
              }
        ClientResponse{ clientResponseData = emptyWhois
                      , clientResponseRaw
                      } <- runRPC $ getWhois queryReq
        let queryRespCode = clientResponseRaw ^. Response._queryCode
        -- storage failure
        queryRespCode `shouldBe` 1
        -- empty whois (defaults)
        whoisPrice emptyWhois `shouldBe` 0
        whoisOwner emptyWhois `shouldBe` (Address "")
        whoisValue emptyWhois `shouldBe` ""

      it "Can set a name value" $ do
        let msg = SetName satoshi addr1 "goodbye to a world"
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        deliverRespCode `shouldBe` 0
        -- check for changes
        let queryReq = QueryArgs
              { queryArgsData = satoshi
              , queryArgsHeight = 0
              , queryArgsProve = False
              }
        ClientResponse{clientResponseData = foundWhois} <- runRPC $ getWhois queryReq
        whoisValue foundWhois `shouldBe` "goodbye to a world"
        -- eveyrthing else should remain the same
        whoisOwner foundWhois `shouldBe` addr1
        whoisPrice foundWhois `shouldBe` 0

      it "Can fail to set a name" $ do
        -- try to set a name without being the owner
        let msg = SetName satoshi addr2 "goodbye to a world"
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        -- response code 2
        deliverRespCode `shouldBe` 2

      it "Can buy an existing name" $ do
        let msg = BuyName 300 satoshi "hello (again) world" addr2
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        deliverRespCode `shouldBe` 0
        -- check for ownership changes
        let queryReq = QueryArgs
              { queryArgsData = satoshi
              , queryArgsHeight = 0
              , queryArgsProve = False
              }
        ClientResponse{clientResponseData = foundWhois} <- runRPC $ getWhois queryReq
        whoisOwner foundWhois `shouldBe` addr2
        whoisPrice foundWhois `shouldBe` 300
        whoisValue foundWhois `shouldBe` "hello (again) world"

      -- -- @TODO: this is a problem
      -- it "Can buy self-owned names without profiting" $ do
      --   let queryReq = QueryArgs
      --         { queryArgsData = addr2
      --         , queryArgsHeight = 0
      --         , queryArgsProve = False
      --         }
      --   ClientResponse{clientResponseData = beforeBuyAmount} <- runRPC $ getBalance queryReq
      --   let msg = BuyName 500 satoshi "hello (again) world" addr2
      --       rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey2 msg
      --       txReq =
      --         RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
      --   deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
      --   let deliverRespCode = deliverResp ^. Response._deliverTxCode
      --   deliverRespCode `shouldBe` 0
      --   let queryReq = QueryArgs
      --         { queryArgsData = addr2
      --         , queryArgsHeight = 0
      --         , queryArgsProve = False
      --         }
      --   ClientResponse{clientResponseData = afterBuyAmount} <- runRPC $ getBalance queryReq
      --   beforeBuyAmount `shouldSatisfy` (_ > afterBuyAmount) afterBuyAmount

      it "Can fail to buy a name" $ do
        -- try to buy at a lower price
        let msg = BuyName 100 satoshi "hello (again) world" addr1
            rawTx = mkSignedRawTransactionWithRoute "nameservice" privateKey1 msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        deliverRespCode `shouldBe` 1

      -- @TODO: make transfer messages
      it "Can fail a transfer" $ do
        -- try to give addr1 2000 from addr2
        pending

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

--------------------------------------------------------------------------------

decodeValue :: HasCodec a => Base64.Base64String -> a
decodeValue = (\(Right a) -> a) . decode . Base64.toBytes

encodeRawTx :: RawTransaction -> Base64.Base64String
encodeRawTx = Base64.fromBytes . Serialize.encode

encodeMsgData :: Message a => a -> BS.ByteString
encodeMsgData = BL.toStrict . toLazyByteString

-- sign a trx with a user's private key
mkSignedRawTransactionWithRoute :: Message a => BS.ByteString -> SecKey -> a -> RawTransaction
mkSignedRawTransactionWithRoute route privateKey msg = sign unsigned
  where unsigned = RawTransaction { rawTransactionData = encodeMsgData msg
                                  , rawTransactionRoute = route
                                  , rawTransactionSignature = ""
                                  }
        sig = signRawTransaction algProxy privateKey unsigned
        sign rt = rt { rawTransactionSignature = Serialize.encode $ exportCompactRecSig sig }

data User = User
  { userPrivKey :: SecKey
  , userAddress :: Address
  }

user0 :: User
user0 = makeUser "0000000000000000000000000000000000000000000000000000000000000000"

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
