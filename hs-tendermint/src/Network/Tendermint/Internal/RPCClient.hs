module Network.Tendermint.Internal.RPCClient where

import           Control.Applicative    ((<|>))
import           Control.Exception      (Exception)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Aeson             (FromJSON (..), ToJSON (..), Value (..),
                                         (.:), (.:?), (.=))
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import           Data.Text              (Text, unpack)
import qualified Network.HTTP.Simple    as HTTP
import           System.Random          (randomIO)

-- | JSON-RPC request.
data Request = Request
  { requestMethod :: !MethodName
  , requestId     :: !Int
  , requestParams :: !Value
  }

instance ToJSON Request where
  toJSON (Request (MethodName method) rid params) = Aeson.object
    [ "jsonrpc" .= String "2.0"
    , "method"  .= method
    , "params"  .= params
    , "id"      .= rid
    ]


-- | JSON-RPC response.
data Response = Response
  { rsResult :: !(Either RpcError Value)
  } deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = Aeson.withObject "JSON-RPC response object" $ \v -> Response
    <$> (Right <$> v .: "result" <|> Left <$> v .: "error")


-- | JSON-RPC error message
data RpcError = RpcError
  { errCode    :: !Int
  , errMessage :: !Text
  , errData    :: !(Maybe Value)
  } deriving Eq

instance Show RpcError where
  show (RpcError code msg dat) =
      "JSON-RPC error " ++ show code ++ ": " ++ unpack msg
        ++ ". Data: " ++ show dat

instance FromJSON RpcError where
  parseJSON = Aeson.withObject "JSON-RPC error object" $ \v -> RpcError
    <$> v .: "code"
    <*> v .: "message"
    <*> v .:? "data"


data JsonRpcException
  = ParsingException String
  | CallException RpcError
  deriving (Eq, Show)

instance Exception JsonRpcException


-- | Name of called method.
newtype MethodName = MethodName Text deriving (Eq, Show)


-- | JSON-RPC client config
data Config = Config
  { cBaseHTTPRequest :: HTTP.Request
  }

defaultConfig :: Config
defaultConfig = Config HTTP.defaultRequest

remote ::
  ( MonadIO m
  , MonadThrow m
  , MonadReader Config m
  , FromJSON output
  , ToJSON input
  )
  => MethodName
  -> input
  -> m output
{-# INLINE remote #-}
remote method input = do
  rid <- abs <$> liftIO randomIO
  let req = Request method rid (toJSON input)
  Config baseHTTPRequest <- ask
  response <- liftIO
    $ HTTP.httpBS
    $ HTTP.setRequestBodyJSON req
    $ HTTP.setRequestHeaders [("Content-Type", "application/json")]
    $ HTTP.setRequestMethod "POST"
    $ baseHTTPRequest
  decodeResponse (HTTP.getResponseBody response)
  where
    decodeResponse
      :: (MonadThrow m, FromJSON a)
      => B.ByteString
      -> m a
    decodeResponse bs = case Aeson.eitherDecodeStrict bs of
      Left err -> throwM $ ParsingException err
      Right (Response (Left rpcError)) -> throwM $ CallException rpcError
      Right (Response (Right resultValue)) -> case Aeson.eitherDecodeStrict $ BL.toStrict $ Aeson.encode resultValue of
        Left err     -> throwM $ ParsingException err
        Right result -> pure result
