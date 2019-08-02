
module Network.ABCI.Types.App where

import           Control.Lens                         ((^.))
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
import           Data.Text                            ()
import           Data.Traversable                     (traverse)
import           Network.ABCI.Types.DecodeError       (DecodeError)
import qualified Network.ABCI.Types.DecodeError       as DecodeError
import           Network.ABCI.Types.Messages.Request  (Request)
import qualified Network.ABCI.Types.Messages.Request  as Request
import           Network.ABCI.Types.Messages.Response (Response)
import qualified Network.ABCI.Types.Messages.Response as Response
import           Network.ABCI.Types.Messages.Types    (MessageType)
import qualified Proto.Types                          as PT
import qualified Proto.Types_Fields                   as PT

-- | Application type that represents a well typed application, i.e. a
-- function from a typed `Request` to a typed `Response`.
newtype App m = App
  { unApp :: forall (t :: MessageType). Request t -> m (Response t) }

-- | Middleware is a component that sits between the server and application.
-- It can do such tasks as logging or response caching. What follows is the general
-- definition of middleware, though a middleware author should feel free to modify this.
type Middleware m = App m -> App m

-- | Transform an application from running in a custom monad to running in `IO`.
transformApp :: (forall a. m a -> g a) -> App m -> App g
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
    onError err = [Response.toProto $ Response.ResponseException $ Response.Exception $ cs $ DecodeError.print err]

    onResponse :: PT.Request'Value -> m PT.Response
    onResponse = Request.withProto $ fmap Response.toProto . app

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
