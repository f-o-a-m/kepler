module Database.IAVL.RPC.Types where

import           Control.Exception           (Exception, throwIO)
import           Data.Text                   (Text, pack)
import           Network.GRPC.Client         (uncompressed)
import           Network.GRPC.Client.Helpers (GrpcClient, GrpcClientConfig (..),
                                              grpcClientConfigSimple,
                                              setupGrpcClient)
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

data GrpcConfig = GrpcConfig
  { grpcHost :: String
  , grpcPort :: Integer
  }

-- | Initialize the GRPC Client
initGrpcClient :: GrpcConfig -> IO GrpcClient
initGrpcClient (GrpcConfig host port) =
-- usually 0.0.0.0:8090
  let grpcClient = grpcClientConfigSimple host (fromInteger port) False
  in  runClientIO (setupGrpcClient (grpcClient{_grpcClientConfigCompression=uncompressed})) >>= \case
        Right gc -> pure gc
        Left err -> throwIO . ClientSetupError . pack $ show err
