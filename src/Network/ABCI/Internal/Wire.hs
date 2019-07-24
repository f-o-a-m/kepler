module Network.ABCI.Internal.Wire
  ( encodeLengthPrefix
  , decodeLengthPrefix
  ) where

import           Data.Bifunctor                (first)
import qualified Data.ByteString               as BS
import           Data.ProtoLens.Encoding.Bytes (getVarInt, putVarInt,
                                                runBuilder, runParser,
                                                signedInt64ToWord,
                                                wordToSignedInt64)
import qualified Network.ABCI.Types.Error      as Error

-- | Transforms a stream of 'ByteString' to a stream of varlength-prefixed
--   'ByteString's
encodeLengthPrefix :: [BS.ByteString] -> BS.ByteString
encodeLengthPrefix = foldMap $ \bytes ->
  let headerN = signedInt64ToWord . fromIntegral . BS.length $ bytes
      header = runBuilder $ putVarInt headerN
  in header `BS.append` bytes
{-# INLINEABLE encodeLengthPrefix #-}

decodeLengthPrefix :: BS.ByteString -> Either Error.Error [BS.ByteString]
decodeLengthPrefix bs
  | bs == mempty = Right []
  | otherwise = do
      n <- first (Error.ProtoLensParseError bs) $ runParser getVarInt bs
      let lengthHeader = runBuilder $ putVarInt n
      messageBytesWithTail <- case BS.stripPrefix lengthHeader bs of
        Nothing -> Left $ Error.InvalidPrefix lengthHeader bs
        Just a  -> Right a
      let (messageBytes, remainder) = BS.splitAt (fromIntegral $ wordToSignedInt64 n) messageBytesWithTail
      (messageBytes : ) <$> decodeLengthPrefix remainder

{-# INLINEABLE decodeLengthPrefix #-}
