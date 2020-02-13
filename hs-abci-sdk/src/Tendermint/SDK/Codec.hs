module Tendermint.SDK.Codec
  ( HasCodec(..)
  , defaultSDKAesonOptions
  ) where

import           Data.Aeson              (Options)
import           Data.Aeson.Casing       (aesonDrop, snakeCase)
import qualified Data.ByteString         as BS
import           Data.String.Conversions (cs)
import           Data.Text               (Text)

-- | This class is used as a codec for all items stored in
-- | the database as well as incoming transaction messages.
class HasCodec a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> Either Text a

instance HasCodec () where
  encode = const ""
  decode = const $ pure ()

instance HasCodec String where
  encode = cs
  decode = Right . cs

instance HasCodec Text where
  encode = cs
  decode = Right . cs

instance HasCodec BS.ByteString where
  encode = id
  decode = Right

defaultSDKAesonOptions :: String -> Options
defaultSDKAesonOptions prefix = aesonDrop (length prefix) snakeCase
