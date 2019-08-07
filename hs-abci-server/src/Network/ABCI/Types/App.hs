
module Network.ABCI.Types.App where

import           Control.Lens                         ((^.),(?~))
import           Control.Monad                        ((>=>))
import           Data.Bifunctor                       (first)
import qualified Data.ByteString                      as BS
import           Data.Function                        ((&))
import qualified Data.ProtoLens                       as PL
import           Data.ProtoLens.Encoding.Bytes        (getVarInt, putVarInt,
                                                       runBuilder, runParser,
                                                       signedInt64ToWord,
                                                       wordToSignedInt64)
import           Data.String.Conversions              (cs)
import           Control.Lens.Wrapped                   (Wrapped (..),
                                                         _Unwrapped')
import           Data.Text                            ()
import           Data.Traversable                     (traverse)
import           Network.ABCI.Types.DecodeError       (DecodeError)
import qualified Network.ABCI.Types.DecodeError       as DecodeError
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Data.Aeson                             (FromJSON (..),
                                                         ToJSON (..))

import           Data.Default.Class                     (Default (..))
import qualified Proto.Types                          as PT
import qualified Proto.Types_Fields                   as PT
import           Data.ProtoLens.Prism                   (( # ))
import           Data.ProtoLens.Message                 (Message (defMessage))

-- | Used to parametrize Request and Respone types
data MessageType
  = MTEcho
  | MTFlush
  | MTInfo
  | MTSetOption
  | MTInitChain
  | MTQuery
  | MTBeginBlock
  | MTCheckTx
  | MTDeliverTx
  | MTEndBlock
  | MTCommit


--------------------------------------------------------------------------------
-- Request
--------------------------------------------------------------------------------

-- Note: that there are 3 type of connection made by tendermint to the ABCI application:
-- * Info/Query Connection, sends only: Echo, Info and SetOption requests
-- * Mempool Connection, sends only: CheckTx and Flush requests
-- * Consensus Connection, InitChain,: BeginBlock, DeliverTx, EndBlock and  Commit requests
-- https://github.com/tendermint/tendermint/blob/v0.32.2/proxy/app_conn.go#L11-L41
data Request (m :: MessageType) :: * where
  -- Info/Query Connection
  RequestEcho :: Request.Echo -> Request 'MTEcho
  RequestInfo :: Request.Info -> Request 'MTInfo
  RequestSetOption :: Request.SetOption -> Request 'MTSetOption
  RequestQuery :: Request.Query -> Request 'MTQuery
  -- Mempool Connection
  RequestCheckTx :: Request.CheckTx -> Request 'MTCheckTx
  RequestFlush :: Request.Flush -> Request 'MTFlush
  -- Consensus Connection
  RequestInitChain :: Request.InitChain -> Request 'MTInitChain
  RequestBeginBlock :: Request.BeginBlock -> Request 'MTBeginBlock
  RequestDeliverTx :: Request.DeliverTx -> Request 'MTDeliverTx
  RequestEndBlock :: Request.EndBlock -> Request 'MTEndBlock
  RequestCommit :: Request.Commit -> Request 'MTCommit

instance ToJSON (Request (t :: MessageType)) where
  toJSON (RequestEcho v)       = toJSON v
  toJSON (RequestInfo v)       = toJSON v
  toJSON (RequestSetOption v)  = toJSON v
  toJSON (RequestQuery v)      = toJSON v
  toJSON (RequestCheckTx v)    = toJSON v
  toJSON (RequestFlush v)      = toJSON v
  toJSON (RequestInitChain v)  = toJSON v
  toJSON (RequestBeginBlock v) = toJSON v
  toJSON (RequestDeliverTx v)  = toJSON v
  toJSON (RequestEndBlock v)   = toJSON v
  toJSON (RequestCommit v)     = toJSON v


instance FromJSON (Request 'MTEcho) where
  parseJSON = fmap RequestEcho . parseJSON
instance FromJSON (Request 'MTInfo) where
  parseJSON = fmap RequestInfo . parseJSON
instance FromJSON (Request 'MTSetOption) where
  parseJSON = fmap RequestSetOption . parseJSON
instance FromJSON (Request 'MTQuery) where
  parseJSON = fmap RequestQuery . parseJSON
instance FromJSON (Request 'MTCheckTx) where
  parseJSON = fmap RequestCheckTx . parseJSON
instance FromJSON (Request 'MTFlush) where
  parseJSON = fmap RequestFlush . parseJSON
instance FromJSON (Request 'MTInitChain) where
  parseJSON = fmap RequestInitChain . parseJSON
instance FromJSON (Request 'MTBeginBlock) where
  parseJSON = fmap RequestBeginBlock . parseJSON
instance FromJSON (Request 'MTDeliverTx) where
  parseJSON = fmap RequestDeliverTx . parseJSON
instance FromJSON (Request 'MTEndBlock) where
  parseJSON = fmap RequestEndBlock . parseJSON
instance FromJSON (Request 'MTCommit) where
  parseJSON = fmap RequestCommit . parseJSON


withProto
  :: (forall (t :: MessageType). Request t -> a)
  -> PT.Request'Value
  -> a
withProto f value = case value of
  PT.Request'Echo echo -> f $ RequestEcho $ echo ^. _Unwrapped'
  PT.Request'Flush flush -> f $ RequestFlush $ flush ^. _Unwrapped'
  PT.Request'Info info -> f $ RequestInfo $ info ^. _Unwrapped'
  PT.Request'SetOption setOption -> f $ RequestSetOption $ setOption ^. _Unwrapped'
  PT.Request'InitChain initChain -> f $ RequestInitChain $ initChain ^. _Unwrapped'
  PT.Request'Query query -> f $ RequestQuery $ query ^. _Unwrapped'
  PT.Request'BeginBlock beginBlock -> f $ RequestBeginBlock $ beginBlock ^. _Unwrapped'
  PT.Request'CheckTx checkTx -> f $ RequestCheckTx $ checkTx ^. _Unwrapped'
  PT.Request'DeliverTx deliverTx -> f $ RequestDeliverTx $ deliverTx ^. _Unwrapped'
  PT.Request'EndBlock endBlock -> f $ RequestEndBlock $ endBlock ^. _Unwrapped'
  PT.Request'Commit commit -> f $ RequestCommit $ commit ^. _Unwrapped'

--------------------------------------------------------------------------------
-- Response
--------------------------------------------------------------------------------

data Response (m :: MessageType) :: * where
  ResponseEcho :: Response.Echo -> Response 'MTEcho
  ResponseFlush :: Response.Flush -> Response 'MTFlush
  ResponseInfo :: Response.Info -> Response 'MTInfo
  ResponseSetOption :: Response.SetOption -> Response 'MTSetOption
  ResponseInitChain :: Response.InitChain -> Response 'MTInitChain
  ResponseQuery :: Response.Query -> Response 'MTQuery
  ResponseBeginBlock :: Response.BeginBlock -> Response 'MTBeginBlock
  ResponseCheckTx :: Response.CheckTx -> Response 'MTCheckTx
  ResponseDeliverTx :: Response.DeliverTx -> Response 'MTDeliverTx
  ResponseEndBlock :: Response.EndBlock -> Response 'MTEndBlock
  ResponseCommit :: Response.Commit -> Response 'MTCommit
  ResponseException :: forall (m :: MessageType) . Response.Exception -> Response m

instance ToJSON (Response (t :: MessageType)) where
  toJSON (ResponseEcho v)       = toJSON v
  toJSON (ResponseFlush v)      = toJSON v
  toJSON (ResponseInfo v)       = toJSON v
  toJSON (ResponseSetOption v)  = toJSON v
  toJSON (ResponseInitChain v)  = toJSON v
  toJSON (ResponseQuery v)      = toJSON v
  toJSON (ResponseBeginBlock v) = toJSON v
  toJSON (ResponseCheckTx v)    = toJSON v
  toJSON (ResponseDeliverTx v)  = toJSON v
  toJSON (ResponseEndBlock v)   = toJSON v
  toJSON (ResponseCommit v)     = toJSON v
  toJSON (ResponseException v)  = toJSON v

instance FromJSON (Response 'MTEcho) where
  parseJSON = fmap ResponseEcho . parseJSON
instance FromJSON (Response 'MTFlush) where
  parseJSON = fmap ResponseFlush . parseJSON
instance FromJSON (Response 'MTInfo) where
  parseJSON = fmap ResponseInfo . parseJSON
instance FromJSON (Response 'MTSetOption) where
  parseJSON = fmap ResponseSetOption . parseJSON
instance FromJSON (Response 'MTInitChain) where
  parseJSON = fmap ResponseInitChain . parseJSON
instance FromJSON (Response 'MTQuery) where
  parseJSON = fmap ResponseQuery . parseJSON
instance FromJSON (Response 'MTBeginBlock) where
  parseJSON = fmap ResponseBeginBlock . parseJSON
instance FromJSON (Response 'MTCheckTx) where
  parseJSON = fmap ResponseCheckTx . parseJSON
instance FromJSON (Response 'MTDeliverTx) where
  parseJSON = fmap ResponseDeliverTx . parseJSON
instance FromJSON (Response 'MTEndBlock) where
  parseJSON = fmap ResponseEndBlock . parseJSON
instance FromJSON (Response 'MTCommit) where
  parseJSON = fmap ResponseCommit . parseJSON

instance Default (Response 'MTEcho) where
  def = ResponseEcho def
instance Default (Response 'MTFlush) where
  def = ResponseFlush def
instance Default (Response 'MTInfo) where
  def = ResponseInfo def
instance Default (Response 'MTSetOption) where
  def = ResponseSetOption def
instance Default (Response 'MTInitChain) where
  def = ResponseInitChain def
instance Default (Response 'MTQuery) where
  def = ResponseQuery def
instance Default (Response 'MTBeginBlock) where
  def = ResponseBeginBlock def
instance Default (Response 'MTCheckTx) where
  def = ResponseCheckTx def
instance Default (Response 'MTDeliverTx) where
  def = ResponseDeliverTx def
instance Default (Response 'MTEndBlock) where
  def = ResponseEndBlock def
instance Default (Response 'MTCommit) where
  def = ResponseCommit def

-- | Translates type-safe 'Response' GADT to the unsafe
--   auto-generated 'Proto.Response'
toProto :: Response t -> PT.Response
toProto r = case r of
  ResponseEcho msg       -> wrap PT._Response'Echo msg
  ResponseFlush msg      -> wrap PT._Response'Flush msg
  ResponseInfo msg       -> wrap PT._Response'Info msg
  ResponseSetOption msg  -> wrap PT._Response'SetOption msg
  ResponseInitChain msg  -> wrap PT._Response'InitChain msg
  ResponseQuery msg      -> wrap PT._Response'Query msg
  ResponseBeginBlock msg -> wrap PT._Response'BeginBlock msg
  ResponseCheckTx msg    -> wrap PT._Response'CheckTx msg
  ResponseDeliverTx msg  -> wrap PT._Response'DeliverTx msg
  ResponseEndBlock msg   -> wrap PT._Response'EndBlock msg
  ResponseCommit msg     -> wrap PT._Response'Commit msg
  ResponseException msg  -> wrap PT._Response'Exception msg
  where
    wrap v msg = defMessage & PT.maybe'value ?~ v # (msg ^. _Wrapped')


-- | Application type that represents a well typed application, i.e. a
-- function from a typed `Request` to a typed `Response`.
newtype App m = App
  { unApp :: forall (t :: MessageType). Request t -> m (Response t) }

-- | Middleware is a component that sits between the server and application.
-- It can do such tasks as logging or response caching. What follows is the general
-- definition of middleware, though a middleware author should feel free to modify this.
type Middleware m = App m -> App m

-- | Transform an application from running in a custom monad to running in `IO`.
transformApp :: (forall (t :: MessageType). m (Response t) -> g (Response t)) -> App m -> App g
transformApp nat (App f) = App $ nat . f

-- | Compiles `App` down to `AppBS`
runApp :: forall m. Applicative m => App m -> LPByteStrings -> m LPByteStrings
runApp (App app) bs =
  bs
    & (decodeLengthPrefix >=> decodeRequests)
    & either (pure . onError) (traverse onResponse)
    & fmap (encodeLengthPrefix . encodeResponses)
  where
    onError :: DecodeError -> [PT.Response]
    onError err = [toProto $ ResponseException $ Response.Exception $ cs $ DecodeError.print err]

    onResponse :: PT.Request'Value -> m PT.Response
    onResponse = withProto $ fmap toProto . app

    -- | Encodes responses to bytestrings
    encodeResponses :: [PT.Response] -> [BS.ByteString]
    encodeResponses = map PL.encodeMessage

    -- | Decodes bytestrings into requests
    decodeRequests :: [BS.ByteString] -> Either DecodeError [PT.Request'Value]
    decodeRequests = traverse $ \packet -> case PL.decodeMessage packet of
      Left parseError -> Left $ DecodeError.CanNotDecodeRequest packet parseError
      Right (request :: PT.Request) -> case request ^. PT.maybe'value of
        Nothing -> Left $ DecodeError.NoValueInRequest packet (request ^. PL.unknownFields)
        Just value -> Right $ value


-- | ByteString which contains multiple length prefixed ByteStrings
newtype LPByteStrings = LPByteStrings { unLPByteStrings :: BS.ByteString } deriving (Ord,Eq)

-- | Encodes ByteStrings into varlength-prefixed ByteString
encodeLengthPrefix :: [BS.ByteString] -> LPByteStrings
encodeLengthPrefix = LPByteStrings . foldMap encoder
  where
    encoder bytes =
      let
        headerN = signedInt64ToWord . fromIntegral . BS.length $ bytes
        header = runBuilder $ putVarInt headerN
      in
        header `BS.append` bytes

-- | Decodes varlength-prefixed ByteString into ByteStrings
decodeLengthPrefix :: LPByteStrings -> Either DecodeError [BS.ByteString]
decodeLengthPrefix (LPByteStrings bs)
  | bs == mempty = Right []
  | otherwise = do
      n <- first (DecodeError.ProtoLensParseError bs) $ runParser getVarInt bs
      let lengthHeader = runBuilder $ putVarInt n
      messageBytesWithTail <- case BS.stripPrefix lengthHeader bs of
        Nothing -> Left $ DecodeError.InvalidPrefix lengthHeader bs
        Just a  -> Right a
      let (messageBytes, remainder) = BS.splitAt (fromIntegral $ wordToSignedInt64 n) messageBytesWithTail
      (messageBytes : ) <$> decodeLengthPrefix (LPByteStrings remainder)
