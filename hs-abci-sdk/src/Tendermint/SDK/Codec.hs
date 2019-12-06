module Tendermint.SDK.Codec
  ( HasCodec(..)
  , defaultSDKAesonOptions
  ) where

import           Data.Aeson        (Options)
import           Data.Aeson.Casing (aesonDrop, snakeCase)
import qualified Data.ByteString   as BS
import           Data.Text         (Text)

-- | This class is used as a codec for all items stored in
-- | the database as well as incoming transaction messages.
class HasCodec a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> Either Text a

defaultSDKAesonOptions :: String -> Options
defaultSDKAesonOptions prefix = aesonDrop (length prefix) snakeCase
