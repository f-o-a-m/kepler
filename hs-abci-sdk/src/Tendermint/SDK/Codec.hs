module Tendermint.SDK.Codec where

import qualified Data.ByteString as BS
import           Data.Text       (Text)

-- | This class is used as a codec for all items stored in
-- | the database.
class HasCodec a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> Either Text a
