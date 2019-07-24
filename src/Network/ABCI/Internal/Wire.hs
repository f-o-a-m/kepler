module Network.ABCI.Internal.Wire
  ( encodeLengthPrefixC
  , decodeLengthPrefixC
  ) where

import           Data.Bifunctor                (first)
import qualified Data.ByteString               as BS
import           Data.Conduit                  (ConduitT, awaitForever, yield)
import qualified Data.Conduit.List             as CL
import           Data.ProtoLens.Encoding.Bytes (getVarInt, putVarInt,
                                                runBuilder, runParser,
                                                signedInt64ToWord,
                                                wordToSignedInt64)
import qualified Network.ABCI.Types.Error      as Error

-- | Transforms a stream of 'ByteString' to a stream of varlength-prefixed
--   'ByteString's
encodeLengthPrefixC
  :: Monad m
  => ConduitT [BS.ByteString] BS.ByteString m ()
encodeLengthPrefixC = CL.map $ foldMap encodeLengthPrefix
  where
    encodeLengthPrefix bytes =
      let headerN = signedInt64ToWord . fromIntegral . BS.length $ bytes
          header = runBuilder $ putVarInt headerN
      in header `BS.append` bytes
{-# INLINEABLE encodeLengthPrefixC #-}

decodeLengthPrefixC
  :: Monad m
  => ConduitT BS.ByteString (Either Error.Error [BS.ByteString]) m ()
decodeLengthPrefixC = awaitForever $ yield . splitOffMessages
  where
    splitOffMessages :: BS.ByteString -> Either Error.Error [BS.ByteString]
    splitOffMessages bs
      | bs == mempty = Right []
      | otherwise = do
          n <- first (Error.ProtoLensParseError bs) $ runParser getVarInt bs
          let lengthHeader = runBuilder $ putVarInt n
          messageBytesWithTail <- case BS.stripPrefix lengthHeader bs of
            Nothing -> Left $ Error.InvalidPrefix lengthHeader bs
            Just a  -> Right a
          let (messageBytes, remainder) = BS.splitAt (fromIntegral $ wordToSignedInt64 n) messageBytesWithTail
          (messageBytes : ) <$> splitOffMessages remainder

{-# INLINEABLE decodeLengthPrefixC #-}
