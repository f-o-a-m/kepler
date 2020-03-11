module KVStore.Test.KVSpec (spec) where

import           Control.Concurrent                     (forkIO)
import           Control.Concurrent.MVar                (MVar, modifyMVar_,
                                                         newMVar, readMVar)
import           Control.Lens                           ((^.))
import           Control.Monad                          (replicateM)
import           Control.Monad.Catch                    (try)
import           Control.Monad.IO.Class                 (liftIO)
import           Control.Monad.Trans.Resource           (runResourceT)
import qualified Data.Aeson                             as A
import           Data.Aeson.Encode.Pretty               (encodePretty)
import           Data.ByteArray.Base64String            (Base64String)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteArray.HexString               as Hex
import           Data.ByteString                        (ByteString)
import           Data.Conduit                           (awaitForever,
                                                         runConduit, (.|))
import           Data.Default.Class                     (def)
import           Data.Either                            (isRight)
import           Data.HashSet                           (fromList)
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import qualified Network.ABCI.Types.Messages.FieldTypes as FieldTypes
import qualified Network.ABCI.Types.Messages.Response   as Response
import qualified Network.Tendermint.Client              as RPC
import           System.Random                          (randomIO)
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
        a <- replicateM 10 $ randomIO @Char
        addEventToCheck tenv "name"
        let asyncTxReq = RPC.RequestBroadcastTxAsync { RPC.requestBroadcastTxAsyncTx = encodeTx $ "name=" <> a }
        -- async returns nothing
        resp <- runRPC $ RPC.broadcastTxAsync asyncTxReq
        RPC.resultBroadcastTxCode resp `shouldBe` 0

      it "Can submit a sync tx and the response code is 0 (success)" $ \tenv -> do
        a <- replicateM 10 $ randomIO @Char
        addEventToCheck tenv "name"
        let txReq = RPC.RequestBroadcastTxSync { RPC.requestBroadcastTxSyncTx = encodeTx $ "name=" <> a }
        -- sync only returns a CheckTx
        resp <- runRPC $ RPC.broadcastTxSync txReq
        RPC.resultBroadcastTxCode resp `shouldBe` 0

      it "Can submit a commit tx, make sure the response code is 0 (success), and get the result(s)" $ \tenv -> do
        -- /broadcast_tx_commit
        -- set name key
        addEventToCheck tenv "name"
        a <- replicateM 10 $ randomIO @Char
        let broadcastTxReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx =  encodeTx $ "name=" <> a }
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
        let foundName = queryResp ^. Response._queryValue
            foundNameWProof = queryRespWProof ^. Response._queryValue
        decodeQuery foundName `shouldBe` a
        decodeQuery foundNameWProof `shouldBe` a
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


      it "Can monitor all events" $ \(TestEnv mvex mvres _) -> do
        expected <- readMVar mvex
        res <- readMVar mvres
        fromList (map A.toJSON expected) `shouldBe` fromList (map A.toJSON res)


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
mkAppEvent :: String -> FieldTypes.Event
mkAppEvent k = FieldTypes.Event
  { eventType = "app"
  , eventAttributes =
    [ FieldTypes.KVPair (encode "creator") (encode "Cosmoshi Netowoko")
    , FieldTypes.KVPair (encode "key") (encode k)
    ]
  }
  where
    encode = Base64.fromBytes . cs @String @ByteString

encodeTx :: String -> Base64String
encodeTx = Base64.fromBytes . cs @_ @ByteString

decodeQuery :: Base64String -> String
decodeQuery = cs @ByteString . Base64.toBytes

-- Test Init
data TestEnv = TestEnv (MVar [FieldTypes.Event]) (MVar [FieldTypes.Event]) (MVar [Text])

testInit :: IO TestEnv
testInit = TestEnv <$> newMVar [] <*> newMVar [] <*> newMVar []

addEventToCheck :: TestEnv -> String -> IO ()
addEventToCheck (TestEnv mvexpected mvseen mveventTypes) ev  = do
  let appEv = mkAppEvent ev
  modifyMVar_ mvexpected $ pure . (appEv :)
  ses <- readMVar mveventTypes
  let evType = FieldTypes.eventType appEv
  if evType`elem` ses
    then pure ()
    else do
      _ <- startNewListener evType
      modifyMVar_ mveventTypes $ pure . (evType :)
 where
  startNewListener evType =
    let subReq = RPC.RequestSubscribe ("tm.event = 'Tx' AND " <> evType <> " EXISTS")
        eventStorer = awaitForever $ \as ->
          liftIO $ modifyMVar_ mvseen $ \es -> pure $
            RPC.txEventEvents as <> es
        forkTendermintM = forkIO . runRPC . runResourceT .  runConduit
    in forkTendermintM $ RPC.subscribe subReq .| eventStorer
