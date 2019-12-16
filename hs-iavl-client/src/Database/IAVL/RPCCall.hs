module Database.IAVL.RPCCall where

import           Network.GRPC.Client         (RPC (..), RawReply)
import           Network.GRPC.Client.Helpers (GrpcClient, rawUnary)
import           Network.HTTP2.Client        (ClientIO, TooMuchConcurrency)
import Data.ProtoLens.Message (defMessage)

import qualified Proto.Iavl.Api        as Api


hash
  :: GrpcClient
  -> ClientIO (Either TooMuchConcurrency (RawReply Api.HashResponse))
hash gc = rawUnary (RPC :: RPC Api.IAVLService "hash") gc defMessage
