
module Database.IAVL.RPCCallSpec where

import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad               (void)
import           Data.ProtoLens.Message      (defMessage)
import           Database.IAVL.RPCCall
import           Network.GRPC.Client         (RawReply, uncompressed)
import           Network.GRPC.Client.Helpers (GrpcClient, GrpcClientConfig (..),
                                              grpcClientConfigSimple,
                                              setupGrpcClient)
import           Network.HTTP2.Client        (ClientIO, TooMuchConcurrency,
                                              runClientIO)
import qualified Proto.Iavl.Api_Fields       as Api
import           Test.Hspec

spec :: Spec
spec = beforeAll initGrpcClient $ do
  let testKey = "test-key"
      testValue = "test-value"
      rootWithTestKey = "\209)\148\US[p\231<tWo7\222\209\205UX\200D\159\147\159\229\182GG\148\170\169\150\ETBI"
  describe "IAVL RPC calls" $ do

    it "should call `hash` RPC method on empty Iavl store and get empty root hash" $ \gc -> do
      res <- runGrpc $ hash gc
      res ^. Api.rootHash `shouldBe` ""

    it "should call `set` RPC method and get false as result since it does not already exist" $ \gc -> do
      let setReq = defMessage & Api.key .~ testKey
                              & Api.value .~ testValue
      res <- runGrpc $ set gc setReq
      res ^. Api.result `shouldBe` False

    it "should call `get` RPC method and get the expected value" $ \gc -> do
      let getReq = defMessage & Api.key .~ testKey
      res <- runGrpc $ get gc getReq
      res ^. Api.value `shouldBe` testValue

    it "should call `saveVersion` RPC method and get the latest hash" $ \gc -> do
      _ <- runGrpc $ saveVersion gc
      res <- runGrpc $ hash gc
      res ^. Api.rootHash `shouldBe` rootWithTestKey

    it "should call `getWithProof` RPC method and get value from earlier version" $ \gc -> do
      let getReq = defMessage & Api.key .~ testKey
      getRes <- runGrpc $ getWithProof gc getReq
      getRes ^. Api.value `shouldBe` testValue

    it "should call `getVersioned` RPC method and get value from earlier version" $ \gc -> do
      let newVal = "new-value"
          setReq = defMessage & Api.key .~ testKey
                              & Api.value .~ newVal
      res <- runGrpc $ set gc setReq
      res ^. Api.result `shouldBe` True
      _ <- runGrpc $ saveVersion gc

      let getReq = defMessage & Api.key .~ testKey
                              & Api.version .~ 1
      getRes <- runGrpc $ getVersioned gc getReq
      getRes ^. Api.value `shouldBe` testValue

    it "should call `getVersionedWithProof` RPC method and get value from earlier version" $ \gc -> do
      let newVal = "new-value-2"
          setReq = defMessage & Api.key .~ testKey
                              & Api.value .~ newVal
      res <- runGrpc $ set gc setReq
      res ^. Api.result `shouldBe` True
      _ <- runGrpc $ saveVersion gc

      let getReq = defMessage & Api.key .~ testKey
                              & Api.version .~ 1
      getRes <- runGrpc $ getVersionedWithProof gc getReq
      getRes ^. Api.value `shouldBe` testValue

    it "should call `remove` RPC method" $ \gc -> do
      let key = "key-to-remove"
          value = "value-to-remove"
          setReq = defMessage & Api.key .~ key
                              & Api.value .~ value
      res <- runGrpc $ set gc setReq
      res ^. Api.result `shouldBe` False
      _ <- runGrpc $ saveVersion gc

      let removeReq = defMessage & Api.key .~ key
      removeRes <- runGrpc $ remove gc removeReq
      removeRes ^. Api.value `shouldBe` value

      let getReq = defMessage & Api.key .~ key
      getRes <- runGrpc $ get gc getReq
      getRes ^. Api.value `shouldBe` ""

    it "should call `verify` RPC method" $ \gc -> do
      let getReq = defMessage & Api.key .~ testKey
                              & Api.version .~ 1
      getRes <- runGrpc $ getVersionedWithProof gc getReq

      let verifyReq = defMessage & Api.rootHash .~ rootWithTestKey
                                 & Api.proof .~ (getRes ^. Api.proof)
      void . runGrpc $ verify gc verifyReq

    it "should call `verifyItem` RPC method" $ \gc -> do
      let getReq = defMessage & Api.key .~ testKey
                              & Api.version .~ 1
      getRes <- runGrpc $ getVersionedWithProof gc getReq

      let verifyReq = defMessage & Api.rootHash .~ rootWithTestKey
                                 & Api.proof .~ (getRes ^. Api.proof)
                                 & Api.key .~ testKey
                                 & Api.value .~ testValue
      void . runGrpc $ verifyItem gc verifyReq

    it "should call `verifyAbsence` RPC method" $ \gc -> do
      let getReq = defMessage & Api.key .~ testKey
                              & Api.version .~ 1
      getRes <- runGrpc $ getVersionedWithProof gc getReq

      let verifyAbReq = defMessage & Api.rootHash .~ rootWithTestKey
                                   & Api.proof .~ (getRes ^. Api.proof)
                                   & Api.key .~ "non-existing key"
      void . runGrpc $ verifyAbsence gc verifyAbReq

    it "should call `versionExists` RPC method on existing version" $ \gc -> do
      let verExistsReq = defMessage & Api.version .~ 1
      verExistsRes <- runGrpc $ versionExists gc verExistsReq
      verExistsRes ^. Api.result `shouldBe` True

    it "should call `versionExists` RPC method on non-existing version" $ \gc -> do
      let verExistsReq = defMessage & Api.version .~ 25
      verExistsRes <- runGrpc $ versionExists gc verExistsReq
      verExistsRes ^. Api.result `shouldBe` False

    it "should call `has` RPC method" $ \gc -> do
      let hasReq = defMessage & Api.key .~ testKey
                              & Api.version .~ 1
      hasRes <- runGrpc $ has gc hasReq
      hasRes ^. Api.result `shouldBe` True

    it "should call `has` RPC method and fail" $ \gc -> do
      let hasReq = defMessage & Api.key .~ "non-existing-key"
                              & Api.version .~ 1
      hasRes <- runGrpc $ has gc hasReq
      hasRes ^. Api.result `shouldBe` False

    it "should call `deleteVersion` RPC method and get False for non-existing version" $ \gc -> do
      let delVerReq = defMessage & Api.version .~ 1
      let verExistsReq = defMessage & Api.version .~ 1
      delVerRes <- runGrpc $ deleteVersion gc delVerReq
      verExistsRes <- runGrpc $ versionExists gc verExistsReq
      delVerRes ^. Api.rootHash `shouldBe` rootWithTestKey
      verExistsRes ^. Api.result `shouldBe` False

    it "should call `rollback` RPC method" $ \gc -> do
      let key = "key-with-rollback"
          value = "value-with-rollback"
          setReq = defMessage & Api.key .~ key
                              & Api.value .~ value
      res <- runGrpc $ set gc setReq
      res ^. Api.result `shouldBe` False

      let getReq = defMessage & Api.key .~ key
      getRes <- runGrpc $ get gc getReq
      getRes ^. Api.value `shouldBe` value

      _ <- runGrpc $ rollback gc

      getRes' <- runGrpc $ get gc getReq
      getRes' ^. Api.value `shouldBe` ""

initGrpcClient :: IO GrpcClient
initGrpcClient =
  let grpcClient = grpcClientConfigSimple "localhost" 8090 False
  in  runClientIO (setupGrpcClient (grpcClient{_grpcClientConfigCompression=uncompressed})) >>= \case
        Right gc -> pure gc
        _        -> error "Error creating GrpcClient"

runGrpc :: ClientIO (Either TooMuchConcurrency (RawReply a)) -> IO a
runGrpc f = runClientIO f >>= \case
  Right (Right (Right (_, _, Right res))) -> pure res
  Right (Right (Right (_, _, Left err))) -> error ("Error running grpc call: " <> show err)
  Right (Right (Left err)) -> error ("Error running grpc call: " <> show err)
  Right (Left err) -> error ("Error running grpc call: " <> show err)
  Left err -> error ("Error running grpc call: " <> show err)



