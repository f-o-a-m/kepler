
module Database.IAVL.RPCCallSpec where

import           Control.Monad.IO.Class         ( liftIO )
import           Network.GRPC.Client.Helpers    ( grpcClientConfigSimple
                                                , GrpcClient
                                                , setupGrpcClient
                                                )
import           Network.HTTP2.Client           ( runClientIO )
import           Test.Hspec
import           Database.IAVL.RPCCall

spec :: Spec
spec = beforeAll initGrpcClient $ do
  describe "IAVL RPC calls" $ do
    it "should call `hash` RPC method" $ \gc -> do
      Right (Right (Right (_, _, Right hashResp))) <- runClientIO $ hash gc
      liftIO $ print hashResp

initGrpcClient :: IO GrpcClient
initGrpcClient =
  let grpcClient = grpcClientConfigSimple "localhost" 8090 True
  in  runClientIO (setupGrpcClient grpcClient) >>= \case
        Right gc -> pure gc
        _        -> error "Error creating GrpcClient"




