module Network.ABCI.Internal.Wire
  ( encodeLengthPrefixC
  , decodeLengthPrefixC
  ) where

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Base16        as BS16
import           Data.Conduit                  (ConduitT, awaitForever, yield)
import qualified Data.Conduit.List             as CL
import           Data.ProtoLens.Encoding.Bytes (getVarInt, putVarInt,
                                                runBuilder, runParser,
                                                signedInt64ToWord,
                                                wordToSignedInt64)
import           Data.String.Conversions       (cs)

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
            Nothing -> let prefixWithMsg = show (cs . BS16.encode $ lengthHeader :: String , cs . BS16.encode $ bs :: String)
                       in Left $ "prefix not actually a prefix!: " <> prefixWithMsg
            Just a -> pure a
          let (messageBytes, remainder) = BS.splitAt (fromIntegral $ wordToSignedInt64 n) messageBytesWithTail
          (messageBytes : ) <$> splitOffMessages remainder
{-# INLINEABLE decodeLengthPrefixC #-}
