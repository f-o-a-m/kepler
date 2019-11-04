module KVStore.Test.KVSpec where

import Control.Monad (void)
import           Control.Lens                         (to,(^.))
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.Binary                          (encode,decode)
import           Data.ByteArray.Base64String          (Base64String)
import qualified Data.ByteArray.Base64String          as Base64
-- import qualified Data.ByteArray.HexString             as Hex
-- import qualified Data.ByteString                      as BS
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Default.Class                   (def)
-- import           Data.Int                             (Int32)
import           Data.String.Conversions              (cs)
-- import qualified Network.ABCI.Types.Messages.Response as Resp
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Test.Hspec


spec :: Spec
spec = do
  describe "Tendermint KV Store - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    it "Can query /abci_info and parse the result" $
      void . runRPC $ RPC.abciInfo

    it "Can query /block and parse the result" $
      -- @NOTE: this defaults to latest block
      void . runRPC $ RPC.block def

    it "Can submit a tx and make sure the response code is 0 (success)" $ do
      let eName = Base64.fromBytes $ cs @String @ByteString "name=satoshi"
          txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = eName }
      deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $
        RPC.broadcastTxCommit txReq
      let deliverRespCode = deliverResp ^. Response._deliverTxCode
      deliverRespCode `shouldBe` 0

    it "Can query /abci_query with a key and get its value" $ do
      let dName = _
          queryReq = def { RPC.requestABCIQueryData = dName }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      let foundName = queryResp ^. Response._queryValue . to decodeName
      foundName `shouldBe` "satoshi"
      pure ()

    -- it "Can query /tx and parse the result" $ do
    --   pure ()

    -- it "Can query /tx and parse the result with a proof" $ do
    --   pure ()

encodeName :: String -> Base64String
encodeName = Base64.fromBytes . LBS.toStrict . encode
  
decodeName :: Base64String -> String
decodeName = decode . LBS.fromStrict . Base64.toBytes

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")
