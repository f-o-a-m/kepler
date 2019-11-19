module Nameservice.Test.E2ESpec where

import           Control.Lens                             (to, (^.))
import           Crypto.Secp256k1                         (SecKey,
                                                           exportCompactRecSig,
                                                           secKey)
import           Data.Aeson                               (ToJSON)
import           Data.Aeson.Encode.Pretty                 (encodePretty)
import qualified Data.ByteArray.Base64String              as Base64
import qualified Data.ByteArray.HexString                 as Hex
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.Lazy                     as BL
import           Data.Default.Class                       (def)
import           Data.Maybe                               (fromJust)
import           Data.Proxy
import qualified Data.Serialize                           as Serialize
import           Data.String                              (fromString)
import           Data.String.Conversions                  (cs)
import           Nameservice.Modules.Nameservice.Messages (BuyName (..),
                                                           DeleteName (..),
                                                           NameserviceMessage (..),
                                                           SetName (..),
                                                           FaucetAccount (..))
import           Nameservice.Modules.Nameservice.Types    (Name (..),
                                                           Whois (..))
import           Nameservice.Modules.Token                (Amount (..))
import qualified Network.ABCI.Types.Messages.Response     as Response
import qualified Network.Tendermint.Client                as RPC
import           Proto3.Suite                             (Message,
                                                           toLazyByteString)
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Tendermint.SDK.Crypto                    (Secp256k1)
import           Tendermint.SDK.Store                     (rawKey)
import           Tendermint.SDK.Types.Address             (Address (..),
                                                           addressToBytes)
import           Tendermint.SDK.Types.Transaction         (RawTransaction (..),
                                                           signRawTransaction)
import           Test.Hspec

spec :: Spec
spec = do
  let satoshi = Name "satoshi"
      addr1 = Address "0x01"
      addr2 = Address "0x02"

  beforeAll (do faucetAccount addr1; faucetAccount addr2) $
    describe "Nameservice Spec" $ do
      it "Can query /health to make sure the node is alive" $ do
        resp <- runRPC RPC.health
        resp `shouldBe` RPC.ResultHealth

      it "Can query account balances" $ do
        let queryReq = def { RPC.requestABCIQueryPath = Just "token/balance"
                           , RPC.requestABCIQueryData = "0x01"
                           }
        queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
          RPC.abciQuery queryReq
        let foundAmount = queryResp ^. Response._queryValue . to decodeValue
        foundAmount `shouldBe` (Amount 1000)

      it "Can create a name" $ do
        let msg = BuyName 0 satoshi "hello world" addr1
            rawTx = mkSignedRawTransactionWithRoute "nameservice" msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        deliverRespCode `shouldBe` 0

      it "Can query for a name" $ do
        let queryReq = def { RPC.requestABCIQueryPath = Just "nameservice/whois"
                           , RPC.requestABCIQueryData = satoshi ^. rawKey . to Hex.fromBytes
                           }
        queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
          RPC.abciQuery queryReq
        let foundWhois = queryResp ^. Response._queryValue . to decodeValue
        whoisValue foundWhois `shouldBe` "hello world"
        whoisOwner foundWhois `shouldBe` addr1
        whoisPrice foundWhois `shouldBe` 0

      it "Can query for a name that doesn't exist" $ do
        let nope = Name "nope"
            queryReq = def { RPC.requestABCIQueryPath = Just "nameservice/whois"
                           , RPC.requestABCIQueryData = nope ^. rawKey . to Hex.fromBytes
                           }
        queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
          RPC.abciQuery queryReq
        let queryRespCode = queryResp ^. Response._queryCode
        queryRespCode `shouldBe` 1

      it "Can set a name value" $ do
        let msg = SetName satoshi addr1 "goodbye to a world"
            rawTx = mkSignedRawTransactionWithRoute "nameservice" msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        deliverRespCode `shouldBe` 0
        -- check for changes
        let queryReq = def { RPC.requestABCIQueryPath = Just "nameservice/whois"
                           , RPC.requestABCIQueryData = satoshi ^. rawKey . to Hex.fromBytes
                           }
        queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
          RPC.abciQuery queryReq
        let foundWhois = queryResp ^. Response._queryValue . to decodeValue
        whoisValue foundWhois `shouldBe` "goodbye to a world"
        -- eveyrthing else should remain the same
        whoisOwner foundWhois `shouldBe` addr1
        whoisPrice foundWhois `shouldBe` 0

      it "Can buy an existing name" $ do
        let msg = BuyName 300 satoshi "hello (again) world" addr2
            rawTx = mkSignedRawTransactionWithRoute "nameservice" msg
            txReq =
              RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
        deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
        let deliverRespCode = deliverResp ^. Response._deliverTxCode
        deliverRespCode `shouldBe` 0
        -- check for ownership changes
        let queryReq = def { RPC.requestABCIQueryPath = Just "nameservice/whois"
                           , RPC.requestABCIQueryData = satoshi ^. rawKey . to Hex.fromBytes
                           }
        queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
          RPC.abciQuery queryReq
        let foundWhois = queryResp ^. Response._queryValue . to decodeValue
        whoisOwner foundWhois `shouldBe` addr2
        whoisPrice foundWhois `shouldBe` 300
        whoisValue foundWhois `shouldBe` "hello (again) world"

      it "Can fail to buy a name" $ do
        -- try to buy at a lower price
        -- try to buy at a price without having that much
        pending

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

faucetAccount :: Address -> IO ()
faucetAccount addr = do
  let msg = FaucetAccount addr 1000
      rawTx = mkSignedRawTransactionWithRoute "nameservice" msg -- why is this `nameservice` and not `token`?
      txReq =
        RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  _ <- runRPC $ RPC.broadcastTxCommit txReq
  return ()

decodeValue :: HasCodec a => Base64.Base64String -> a
decodeValue = (\(Right a) -> a) . decode . Base64.toBytes

encodeRawTx :: RawTransaction -> Base64.Base64String
encodeRawTx = Base64.fromBytes . Serialize.encode

encodeMsgData :: Message a => a -> BS.ByteString
encodeMsgData = BL.toStrict . toLazyByteString

mkSignedRawTransactionWithRoute :: Message a => BS.ByteString -> a -> RawTransaction
mkSignedRawTransactionWithRoute route msg = sign unsigned
  where unsigned = RawTransaction { rawTransactionData = encodeMsgData msg
                                  , rawTransactionRoute = route
                                  , rawTransactionSignature = ""
                                  }
        sig = signRawTransaction algProxy privateKey unsigned
        sign rt = rt { rawTransactionSignature = Serialize.encode $ exportCompactRecSig sig }

privateKey :: SecKey
privateKey = fromJust . secKey . Hex.toBytes . fromString $
  "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

algProxy :: Proxy Secp256k1
algProxy = Proxy
