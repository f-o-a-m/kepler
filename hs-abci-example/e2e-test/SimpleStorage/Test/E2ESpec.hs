module SimpleStorage.Test.E2ESpec where

import           Control.Lens                         (to, (^.))
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.Binary                          (decode, encode)
import           Data.ByteArray.Base64String          (Base64String)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteArray.HexString             as Hex
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Default.Class                   (def)
import           Data.Int                             (Int32)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Response as Resp
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import qualified SimpleStorage.Modules.SimpleStorage  as SS
import           SimpleStorage.Types
import           Tendermint.SDK.Store
import           Test.Hspec


spec :: Spec
spec = do
  describe "SimpleStorage E2E - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    it "Can fail to query the count when not initialized and make sure the response code is 1 (failure)" $ do
      let queryReq =
            def { RPC.requestABCIQueryPath = Just "count/count"
                , RPC.requestABCIQueryData = SS.CountKey ^. rawKey . to Hex.fromBytes

                }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      let responseCode = queryResp ^. Resp._queryCode
      responseCode `shouldBe` 1

    it "Can submit a tx synchronously and make sure that the response code is 0 (success)" $ do
      let tx = UpdateCountTx "irakli" 4
          txReq = RPC.RequestBroadcastTxCommit
                    { RPC.requestBroadcastTxCommitTx = Base64.fromBytes . encodeAppTxMessage $ ATMUpdateCount tx
                    }
      deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
      let deliverRespCode = deliverResp ^. Response._deliverTxCode
      deliverRespCode `shouldBe` 0

    it "can make sure the synchronous tx transaction worked and the count is now 4" $ do
      let queryReq =
            def { RPC.requestABCIQueryPath = Just "count/count"
                , RPC.requestABCIQueryData = SS.CountKey ^. rawKey . to Hex.fromBytes
                }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      let foundCount = queryResp ^. Resp._queryValue . to decodeCount
      foundCount `shouldBe` 4


encodeCount :: Int32 -> Base64String
encodeCount = Base64.fromBytes . LBS.toStrict . encode

decodeCount :: Base64String -> Int32
decodeCount =  decode . LBS.fromStrict . Base64.toBytes

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")
