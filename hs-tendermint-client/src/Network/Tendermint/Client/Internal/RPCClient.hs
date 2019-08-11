module Network.Tendermint.Client.Internal.RPCClient where

import           Control.Applicative    ((<|>))
import           Control.Exception      (Exception)
import           Control.Monad.Catch    (MonadThrow, throwM)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ask)
import           Data.Aeson             (FromJSON (..), Result (..),
                                         ToJSON (..), Value (..), fromJSON,
                                         (.:), (.:?), (.=))
import qualified Data.Aeson             as Aeson
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
  { responseId     :: !Int
  , responseResult :: !(Either RpcError Value)
  } deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = Aeson.withObject "JSON-RPC response object" $ \v ->
    Response <$> v .: "id"
             <*> (Right <$> v .: "result" <|> Left <$> v .: "error")

-- this instance is usefule for logging
instance ToJSON Response where
  toJSON (Response rid res) = Aeson.object
    [ "jsonrpc" .= String "2.0"
    , "id"      .= rid
    , case res of
        Left e  -> "error" .= e
        Right r -> "result" .= r
    ]

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

instance ToJSON RpcError where
  toJSON (RpcError code msg _data)= Aeson.object
   [ "code" .= code
   , "message" .= msg
   , "data" .= _data
   ]


data JsonRpcException
  = ParsingException String
  | CallException RpcError
  deriving (Eq, Show)

instance Exception JsonRpcException


-- | Name of called method.
newtype MethodName = MethodName Text deriving (Eq, Show, ToJSON)


-- | JSON-RPC client config
data Config = Config
  { cBaseHTTPRequest :: HTTP.Request
  -- ^ A base request used for all JSON RPC requests
  , withRequest      :: Request -> IO ()
  -- ^ An acion to perform before sending the 'HTTP.Request'
  , withResponse     :: Response -> IO ()
  -- ^ An acion to perform before handling the 'HTTP.Response'
  }

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
  Config baseHTTPRequest withReq withResp <- ask
  let req = Request method rid (toJSON input)
      httpReq = HTTP.setRequestBodyJSON req
              $ HTTP.setRequestHeaders [("Content-Type", "application/json")]
              $ HTTP.setRequestMethod "POST"
              $ baseHTTPRequest
  liftIO $ do
    withReq req
    resp <- HTTP.httpBS httpReq
    rpcResponse <- decodeRPCResponse $ HTTP.getResponseBody resp
    withResp rpcResponse
    extractResult rpcResponse
  where
    decodeRPCResponse bs = case Aeson.eitherDecodeStrict bs of
      Left err       -> throwM $ ParsingException err
      Right response -> pure response
    extractResult (Response _ resp) = case resp of
      Left rpcError -> throwM $ CallException rpcError
      Right resultValue ->
        case fromJSON resultValue of
          Error err      -> throwM $ ParsingException err
          Success result -> pure result
