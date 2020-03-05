module KVStore.Test.KVSpec (spec) where

import           Control.Concurrent                   (forkIO)
import           Control.Concurrent.MVar              (MVar, modifyMVar_,
                                                       newMVar)
import           Control.Lens                         (to, (^.))
import           Control.Lens.Fold                    ((^?))
import           Control.Monad                        (void)
import           Control.Monad.Catch                  (try)
import qualified Data.Aeson                           as A
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.Aeson.Lens                      as A
import           Data.ByteArray.Base64String          (Base64String)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteArray.HexString             as Hex
import           Data.ByteString                      (ByteString)
import           Data.Default.Class                   (def)
import           Data.Either                          (isRight)
--import           Data.HashSet                         (difference, fromList)
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Tendermint.SDK.BaseApp.Events        (Event (..), ToEvent (..))
import           Test.Hspec


spec :: Spec
spec = do
  beforeAll testInit $ do
    describe "Tendermint KV Store - via hs-tendermint-client" $ do

      it "Can query /health to make sure the node is alive" $ const $ do
        resp <- runRPC RPC.health
        resp `shouldBe` RPC.ResultHealth

      it "Can query /abci_info and parse the result" $ const $ do
        result :: Either RPC.JsonRpcException RPC.ResultABCIInfo <- try $ runRPC RPC.abciInfo
        result `shouldSatisfy` isRight

      it "Can query /block and parse the result" $ const $ do
        -- @NOTE: this defaults to latest block
        result :: Either RPC.JsonRpcException RPC.ResultBlock <- try $ runRPC (RPC.block def)
        result `shouldSatisfy` isRight

      it "Can submit a async tx and the response code is 0 (success)" $ \tenv -> do
        let asyncTxReq = RPC.RequestBroadcastTxAsync { RPC.requestBroadcastTxAsyncTx = encodeTx "abcd" }
        addEventToCheck tenv $ mkAppEvent "abcd"
        -- async returns nothing
        resp <- runRPC $ RPC.broadcastTxAsync asyncTxReq
        RPC.resultBroadcastTxCode resp `shouldBe` 0

      it "Can submit a sync tx and the response code is 0 (success)" $ \tenv -> do
        let txReq = RPC.RequestBroadcastTxSync { RPC.requestBroadcastTxSyncTx = encodeTx "efgh" }
        addEventToCheck tenv $ mkAppEvent "efgh"
        -- sync only returns a CheckTx
        resp <- runRPC $ RPC.broadcastTxSync txReq
        RPC.resultBroadcastTxCode resp `shouldBe` 0

      it "Can submit a commit tx, make sure the response code is 0 (success), and get the result(s)" $ \tenv -> do
        -- /broadcast_tx_commit
        -- set name key
        let broadcastTxReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeTx "name=satoshi" }
        addEventToCheck tenv $ mkAppEvent "name"
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


      it "Can monitor all events" $ const pending
      --it "Can monitor all events" $ \(TestEnv mvex mvres _) -> do
      --  expected <- readMVar mvex
      --  res <- readMVar mvres
      --  (fromList expected `difference` fromList res) `shouldBe` fromList []

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
          prettyPrint :: forall b. A.ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response") host port tls

-- See https://github.com/tendermint/tendermint/blob/master/abci/example/kvstore/kvstore.go#L101
mkAppEvent :: Text -> App
mkAppEvent k = App "Cosmoshi Netowoko" k

data App = App
  { creator :: Text
  , key     :: Text
  } deriving (Show, Eq, Generic)

instance ToEvent App


-- Test Init
data TestEnv = TestEnv (MVar [A.Value]) (MVar [A.Value]) [Text]

testInit :: IO TestEnv
testInit = do
  expectedEventsMVar <- newMVar []
  resultEventsMVar <- newMVar []
  pure $ TestEnv expectedEventsMVar resultEventsMVar []

addEventToCheck :: ToEvent a => TestEnv -> a -> IO ()
addEventToCheck (TestEnv mvexpected mvres ses) ev = do
  modifyMVar_ mvexpected $ \es -> pure $ es <> [A.toJSON . toEvent $ ev]
  let evType = eventType (toEvent ev)
  if evType`elem` ses
    then pure ()
    else startNewListener evType
 where
  startNewListener evType =
    let subReq = RPC.RequestSubscribe ("tm.event = 'Tx' AND " <> evType <> " EXISTS")
        forkTendermintM = void . forkIO . void . runRPC
    in forkTendermintM $ RPC.subscribe subReq (handler evType)
  handler evType res = case res ^? txEvents of
    Nothing -> pure ()
    Just v -> case A.fromJSON v of
      A.Error _ -> error ("Failed to parse\n" <> cs (A.encode v) )
      A.Success evs ->
        let filterFn v' = evType == eventType v'
            filteredEvs = filter filterFn evs
        in modifyMVar_ mvres $ \es -> pure $ es <> map A.toJSON filteredEvs
  txEvents = A.key "result"
           . A.key "data"
           . A.key "value"
           . A.key "TxResult"
           . A.key "result"
           . A.key "events"
