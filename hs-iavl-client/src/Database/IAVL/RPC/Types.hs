module Database.IAVL.RPC.Types where

import           Control.Exception                    (Exception, throwIO)
import           Network.GRPC.Client         (uncompressed)
import           Network.GRPC.Client.Helpers (GrpcClient, GrpcClientConfig (..),
                                              grpcClientConfigSimple,
                                              setupGrpcClient)
import Data.Text (Text, pack)
import           Network.HTTP2.Client        (runClientIO)


--------------------------------------------------------------------------------
-- | GRPCClientError
--------------------------------------------------------------------------------
-- | This type represents error with the GRPC Client
data GRPCClientError = ClientSetupError Text
  deriving Show

instance Exception GRPCClientError

--------------------------------------------------------------------------------
-- | initGrpcClient
--------------------------------------------------------------------------------
-- | Initialize the GRPC Client
initGrpcClient :: IO GrpcClient
initGrpcClient =
  let grpcClient = grpcClientConfigSimple "localhost" 8090 False
  in  runClientIO (setupGrpcClient (grpcClient{_grpcClientConfigCompression=uncompressed})) >>= \case
        Right gc -> pure gc
        Left err -> throwIO . ClientSetupError . pack $ show err
