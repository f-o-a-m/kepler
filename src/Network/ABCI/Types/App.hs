
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
import           Network.ABCI.Types.DecodeError             (DecodeError)
import qualified Network.ABCI.Types.DecodeError             as DecodeError
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

-- | Transform an application from running in a custom monad to running in `IO`.
transformApp :: (forall a. m a -> g a) -> App m -> App g
transformApp nat (App f) = App $ nat . f

-- | compiles `App` down to `AppBS`
runApp :: Applicative m => App m -> AppBS m
runApp (App app) bs =
  bs
    & (decodeLengthPrefix >=> decodeRequests)
    & either (pure . onError) (traverse onResponce)
    & fmap (encodeLengthPrefix . encodeResponces)
  where
    onError :: DecodeError -> [PT.Response]
    onError err = [Response.toProto $ Response.ResponseException $ Response.Exception $ cs $ DecodeError.print err]
    -- for some reason with this type we get • Couldn't match type ‘m1’ with ‘m’
    -- onResponce :: PT.Request'Value -> m PT.Response
    onResponce = Request.withProto $ fmap Response.toProto . app

-- | ByteString which contains multiple length prefixed messages
type LengthPrefixedByteString = BS.ByteString

-- | compiled Application which operates on varlength-prefixed ByteString
type AppBS m = LengthPrefixedByteString -> m LengthPrefixedByteString


-- | Encodes responces to bytestrings
encodeResponces :: [PT.Response] -> [BS.ByteString]
encodeResponces = map PL.encodeMessage

-- | Decodes bytestrings into requests
decodeRequests :: [BS.ByteString] -> Either DecodeError [PT.Request'Value]
decodeRequests = traverse $ \packet -> case PL.decodeMessage packet of
  Left parseError -> Left $ DecodeError.CanNotDecodeRequest packet parseError
  Right (request :: PT.Request) -> case request ^. PT.maybe'value of
    Nothing -> Left $ DecodeError.NoValueInRequest packet (request ^. PL.unknownFields)
    Just value -> Right $ value

-- | Encodes ByteStrings into varlength-prefixed ByteString
encodeLengthPrefix :: [BS.ByteString] -> LengthPrefixedByteString
encodeLengthPrefix = foldMap $ \bytes ->
  let headerN = signedInt64ToWord . fromIntegral . BS.length $ bytes
      header = runBuilder $ putVarInt headerN
  in header `BS.append` bytes
{-# INLINEABLE encodeLengthPrefix #-}

-- | Decodes varlength-prefixed ByteString into ByteStrings
decodeLengthPrefix :: LengthPrefixedByteString -> Either DecodeError [BS.ByteString]
decodeLengthPrefix bs
  | bs == mempty = Right []
  | otherwise = do
      n <- first (DecodeError.ProtoLensParseError bs) $ runParser getVarInt bs
      let lengthHeader = runBuilder $ putVarInt n
      messageBytesWithTail <- case BS.stripPrefix lengthHeader bs of
        Nothing -> Left $ DecodeError.InvalidPrefix lengthHeader bs
        Just a  -> Right a
      let (messageBytes, remainder) = BS.splitAt (fromIntegral $ wordToSignedInt64 n) messageBytesWithTail
      (messageBytes : ) <$> decodeLengthPrefix remainder
{-# INLINEABLE decodeLengthPrefix #-}
