module Tendermint.SDK.Codec where

import           Data.Bifunctor          (first)
import qualified Data.ByteString         as BS
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Proto3.Suite            (Message (..), fromByteString,
                                          toLazyByteString)
import           Tendermint.SDK.Format   (coerceProto3Error,
                                          formatMessageParseError)

-- | This class is used as a codec for all items stored in
-- | the database.
class HasCodec a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> Either Text a

    -- default proto3-suite codecs
    default encode :: Message a => a -> BS.ByteString
    encode = cs . toLazyByteString
    default decode :: Message a => BS.ByteString -> Either Text a
    decode = first (formatMessageParseError . coerceProto3Error) . fromByteString
