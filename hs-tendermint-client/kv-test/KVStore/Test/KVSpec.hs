module KVStore.Test.KVSpec (spec) where

import           Control.Lens                         (to, (^.))
import           Control.Monad.Catch                  (try)
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.ByteArray.Base64String          (Base64String)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteArray.HexString             as Hex
import           Data.ByteString                      (ByteString)
import           Data.Default.Class                   (def)
import           Data.Either                          (isRight)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Test.Hspec


spec :: Spec
spec = do
  describe "Tendermint KV Store - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    it "Can query /abci_info and parse the result" $ do
      result :: Either RPC.JsonRpcException RPC.ResultABCIInfo <- try $ runRPC RPC.abciInfo
      result `shouldSatisfy` isRight

    it "Can query /block and parse the result" $ do
      -- @NOTE: this defaults to latest block
      result :: Either RPC.JsonRpcException RPC.ResultBlock <- try $ runRPC (RPC.block def)
      result `shouldSatisfy` isRight

    it "Can submit a async tx and the response code is 0 (success)" $ do
      let asyncTxReq = RPC.RequestBroadcastTxAsync { RPC.requestBroadcastTxAsyncTx = encodeTx "abcd" }
      -- async returns nothing
      resp <- runRPC $ RPC.broadcastTxAsync asyncTxReq
      RPC.resultBroadcastTxCode resp `shouldBe` 0

    it "Can submit a sync tx and the response code is 0 (success)" $ do
      let txReq = RPC.RequestBroadcastTxSync { RPC.requestBroadcastTxSyncTx = encodeTx "efgh" }
      -- sync only returns a CheckTx
      resp <- runRPC $ RPC.broadcastTxSync txReq
      RPC.resultBroadcastTxCode resp `shouldBe` 0

    it "Can submit a commit tx, make sure the response code is 0 (success), and get the result(s)" $ do
      -- /broadcast_tx_commit
      -- set name key
      let broadcastTxReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeTx "name=satoshi" }
      broadcastResp <- runRPC $ RPC.broadcastTxCommit broadcastTxReq
      let deliverResp = RPC.resultBroadcastTxCommitDeliverTx broadcastResp
          deliverRespCode = deliverResp ^. Response._deliverTxCode
      deliverRespCode `shouldBe` 0
      -- /abci_query (w+w/o proof)
      -- get name key value
      let dName = Hex.fromBytes $ cs @String @ByteString "name"
          queryReq = def { RPC.requestABCIQueryData = dName }
          queryReqWProof = def { RPC.requestABCIQueryData = dName
                               , RPC.requestABCIQueryProve = True
                               }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      queryRespWProof <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReqWProof
      let foundName = queryResp ^. Response._queryValue . to decodeName
          foundNameWProof = queryRespWProof ^. Response._queryValue . to decodeName
      foundName `shouldBe` "satoshi"
      foundNameWProof `shouldBe` "satoshi"
      -- check with /tx endpoint (w+w/o proof)
      let hash = RPC.resultBroadcastTxCommitHash $ broadcastResp
          -- convert hex to base64
          baseHash = Base64.fromBytes . Hex.toBytes @ByteString $ hash
          txReq = def { RPC.requestTxHash = Just baseHash }
          txReqWP = RPC.RequestTx { RPC.requestTxHash = Just baseHash
                                  , RPC.requestTxProve = True
                                  }
      -- check the hashes are the same
      txResultHash <- fmap RPC.resultTxHash . runRPC $ RPC.tx txReq
      txResultWPHash <- fmap RPC.resultTxHash . runRPC $ RPC.tx txReqWP
      txResultHash `shouldBe` hash
      txResultWPHash `shouldBe` hash

encodeTx :: String -> Base64String
encodeTx = Base64.fromBytes . cs @String @ByteString

decodeName :: Base64String -> String
decodeName = cs @ByteString @String . Base64.toBytes

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ host port tls = RPC.defaultConfig "localhost" 26657 False
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response") host port tls
