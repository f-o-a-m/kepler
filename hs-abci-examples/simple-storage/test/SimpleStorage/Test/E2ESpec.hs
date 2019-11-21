module SimpleStorage.Test.E2ESpec where

import           Control.Lens                         ((^.))
import           Control.Monad.Reader                 (ReaderT)
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.ByteArray.Base64String          (Base64String)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteArray.HexString             as Hex
import           Data.ByteString                      (ByteString)
import           Data.Default.Class                   (def)
import           Data.Int                             (Int32)
import           Data.Proxy
import           Data.Serialize                       (decode, encode)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import qualified Network.Tendermint.Client            as RPC
import qualified SimpleStorage.Modules.SimpleStorage  as SS
import           SimpleStorage.Types                  (AppTxMessage (..),
                                                       UpdateCountTx (..),
                                                       encodeAppTxMessage)
import           Tendermint.SDK.Query.Client          (ClientResponse (..),
                                                       HasClient (..),
                                                       RunClient (..))
import           Tendermint.SDK.Query.Types           (QueryArgs (..))
import           Test.Hspec


spec :: Spec
spec = do
  describe "SimpleStorage E2E - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    it "Can query the count and make sure its initialized to 0" $ do
      let queryReq = QueryArgs
            { queryArgsData = SS.CountKey
            , queryArgsHeight = 0
            , queryArgsProve = False
            }
      ClientResponse{clientResponseData} <- runQueryRunner $ getCount queryReq
      clientResponseData `shouldBe` SS.Count 0

    it "Can submit a tx synchronously and make sure that the response code is 0 (success)" $ do
      let tx = UpdateCountTx "irakli" 4
          txReq = RPC.RequestBroadcastTxCommit
                    { RPC.requestBroadcastTxCommitTx = Base64.fromBytes . encodeAppTxMessage $ ATMUpdateCount tx
                    }
      deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
      let deliverRespCode = deliverResp ^. Resp._deliverTxCode
      deliverRespCode `shouldBe` 0

    it "can make sure the synchronous tx transaction worked and the count is now 4" $ do
      let queryReq = QueryArgs
            { queryArgsData = SS.CountKey
            , queryArgsHeight = 0
            , queryArgsProve = False
            }
      ClientResponse{clientResponseData} <- runQueryRunner $ getCount queryReq
      clientResponseData `shouldBe` SS.Count 4


encodeCount :: Int32 -> Base64String
encodeCount = Base64.fromBytes  . encode

decodeCount :: Base64String -> Int32
decodeCount =  (\(Right a) -> a) . decode . Base64.toBytes

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")

newtype QueryRunner a = QueryRunner
  {_runQueryRunner :: ReaderT RPC.Config IO a}
  deriving (Functor, Applicative, Monad)

runQueryRunner :: QueryRunner a -> IO a
runQueryRunner = runRPC . _runQueryRunner

instance RunClient QueryRunner where
  runQuery Req.Query{..} =
    let rpcQ = RPC.RequestABCIQuery
          { RPC.requestABCIQueryPath = Just queryPath
          , RPC.requestABCIQueryData = Hex.fromBytes @ByteString . Base64.toBytes $ queryData
          , RPC.requestABCIQueryHeight = Just $ queryHeight
          , RPC.requestABCIQueryProve  = queryProve
          }
    in RPC.resultABCIQueryResponse <$> QueryRunner (RPC.abciQuery rpcQ)

getCount :: QueryArgs SS.CountKey -> QueryRunner (ClientResponse SS.Count)
getCount = genClient (Proxy :: Proxy QueryRunner) (Proxy :: Proxy SS.Api) def