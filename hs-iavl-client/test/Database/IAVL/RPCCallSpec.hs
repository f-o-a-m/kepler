
module Database.IAVL.RPCCallSpec where

import           Network.GRPC.Client.Helpers    ( grpcClientConfigSimple
                                                , GrpcClient
                                                , GrpcClientConfig(..)
                                                , setupGrpcClient
                                                )
import Control.Lens ((&), (.~), (^.))
import Data.ProtoLens.Message (defMessage)
import           Network.GRPC.Client    ( uncompressed, RawReply)
import           Network.HTTP2.Client           ( runClientIO, ClientIO, TooMuchConcurrency )
import           Test.Hspec
import           Database.IAVL.RPCCall
import qualified Proto.Iavl.Api_Fields        as Api

spec :: Spec
spec = beforeAll initGrpcClient $ do
  describe "IAVL RPC calls" $ do
    it "should call `hash` RPC method on empty Iavl store and get empty root hash" $ \gc -> do
      res <- runGrpc $ hash gc
      res ^. Api.rootHash `shouldBe` ""

    it "should call `set` RPC method and get false as result since it does not already exist" $ \gc -> do
      let setReq = defMessage & Api.key .~ "test-key"
                              & Api.value .~ "test-value"
      res <- runGrpc $ set gc setReq
      res ^. Api.result `shouldBe` False

    it "should call `get` RPC method and get the expected value" $ \gc -> do
      let getReq = defMessage & Api.key .~ "test-key"
      res <- runGrpc $ get gc getReq
      res ^. Api.value `shouldBe` "test-value"

    it "should call `saveVersion` RPC method and get the latest hash" $ \gc -> do
      _ <- runGrpc $ saveVersion gc
      res <- runGrpc $ hash gc
      res ^. Api.rootHash `shouldBe` "\209)\148\US[p\231<tWo7\222\209\205UX\200D\159\147\159\229\182GG\148\170\169\150\ETBI"

    it "should call `saveVersion` RPC method and get the latest hash" $ \gc -> do
      _ <- runGrpc $ saveVersion gc
      res <- runGrpc $ hash gc
      res ^. Api.rootHash `shouldBe` "\209)\148\US[p\231<tWo7\222\209\205UX\200D\159\147\159\229\182GG\148\170\169\150\ETBI"

    it "should call `deleteVersion` RPC method and get the latest hash as empty" $ \gc -> do
      pending
      let delVerReq = defMessage & Api.version .~ 1
      delVerRes <- runGrpc $ deleteVersion gc delVerReq
      delVerRes ^. Api.rootHash `shouldBe` "\209)\148\US[p\231<tWo7\222\209\205UX\200D\159\147\159\229\182GG\148\170\169\150\ETBI"
      _ <- runGrpc $ saveVersion gc
      res <- runGrpc $ hash gc
      res ^. Api.rootHash `shouldBe` ""

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



