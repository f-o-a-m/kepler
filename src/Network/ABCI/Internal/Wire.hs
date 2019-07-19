{- This module implements the wire protocol related functionality.

Note that we mark everything we expose as INLINEABLE so that the optimizer
can see more code when the 'Conduit' parts fuse.

For an explanation of what Stream Fusion is see:

https://www.fpcomplete.com/blog/2014/08/conduit-stream-fusion
https://pdfs.semanticscholar.org/64d2/a65a7d559f9b05570fb0fea8bb4cccd83ae2.pdf

FIXME: we should check core and/or benchmark this to make sure fusion is
really happening and change to INLINE to force inlining if it doesn't

-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.ABCI.Internal.Wire (
  encodeLengthPrefixC
, decodeLengthPrefixC
, beWordFromBytes
) where


import           Data.Bits                     (shiftL)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as BS16
import           Data.Conduit                  (ConduitT, awaitForever, yield)
import qualified Data.Conduit.List             as CL
import           Data.ProtoLens.Encoding.Bytes (getVarInt, putVarInt,
                                                runBuilder, runParser,
                                                signedInt64ToWord,
                                                wordToSignedInt64)
import           Data.String.Conversions       (cs)
import           Data.Word                     (Word64)
import           Text.Printf                   (printf)

-- | Transforms a stream of 'ByteString' to a stream of varlength-prefixed
--   'ByteString's
encodeLengthPrefixC
  :: Monad m => ConduitT [BS.ByteString] BS.ByteString m ()
encodeLengthPrefixC = CL.map $ foldMap encodeLengthPrefix
  where
    encodeLengthPrefix bytes =
      let headerN = signedInt64ToWord . fromIntegral . BS.length $ bytes
          header = runBuilder (putVarInt headerN)
      in header `BS.append` bytes
{-# INLINEABLE encodeLengthPrefixC #-}

decodeLengthPrefixC
  :: Monad m
  => ConduitT BS.ByteString (Either String [BS.ByteString]) m ()
decodeLengthPrefixC = awaitForever $ \bytes ->
  case splitOffMessages bytes of
    Left err       -> yield (Left err)
    Right messages -> yield $ Right messages
  where
    splitOffMessages :: BS.ByteString -> Either String [BS.ByteString]
    splitOffMessages bs
      | bs == mempty = pure []
      | otherwise = do
          n <- runParser getVarInt bs
          let lengthHeader = runBuilder $ putVarInt n
          messageBytesWithTail <- case BS.stripPrefix lengthHeader bs of
            Nothing -> let prefixWithMsg = show @(String, String) (cs . BS16.encode $ lengthHeader, cs . BS16.encode $ bs)
                       in Left $ "prefix not actually a prefix!: " <> prefixWithMsg
            Just a -> pure a
          let (messageBytes, remainder) = BS.splitAt (fromIntegral $ wordToSignedInt64 n) messageBytesWithTail
          (messageBytes : ) <$> splitOffMessages remainder

{-# INLINEABLE decodeLengthPrefixC #-}


-- Decodes a 'Word64' from an arbitrary length big-endian encoded 'ByteString'
-- of length <= 8 bytes.
--
-- Note: We could actually remove the length <= 8 bytes limitation by
-- decoding into an arbitrary size 'Integer' (slower) type.
-- However, the golang implementation also suffers from this "limitation" so
-- I assume this is expected behaviour.
-- In any case, we use this to decode the length of the
-- 'ByteString' we're receiving from the wire, anything that can't fit in
-- a 64bit is ridiculously large and will almost certainly consume all
-- available memory.
beWordFromBytes :: BS.ByteString -> Either String Word64
beWordFromBytes s | BS.length s > 8 =
  Left (printf "Max message size is 8 bytes, got %d" (BS.length s))
beWordFromBytes s = Right (snd (BS.foldr' step (0,0) s)) where
  step w (!pos,!acc) = (pos+1, acc + (fromIntegral w `shiftL` (pos*8)))
{-# INLINEABLE beWordFromBytes #-}
