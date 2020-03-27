module Tendermint.SDK.Codec
  ( HasCodec(..)
  , defaultSDKAesonOptions
  ) where

import           Data.Aeson                    (Options)
import           Data.Aeson.Casing             (aesonDrop, snakeCase)
import           Data.Bifunctor                (first)
import qualified Data.ByteString               as BS
import           Data.Int                      (Int32, Int64)
import qualified Data.ProtoLens.Encoding.Bytes as PB
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Data.Word                     (Word32, Word64)



-- | This class is used as a codec for all items stored in
-- | the database as well as incoming transaction messages.
class HasCodec a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> Either Text a

instance HasCodec () where
  encode = const ""
  decode = const $ pure ()


instance HasCodec Word32 where
  encode = PB.runBuilder . PB.putFixed32
  decode = first cs . PB.runParser PB.getFixed32

instance HasCodec Int32 where
  encode = PB.runBuilder . PB.putFixed32 . PB.signedInt32ToWord
  decode = first cs . PB.runParser (PB.wordToSignedInt32 <$> PB.getFixed32)

instance HasCodec Word64 where
  encode = PB.runBuilder . PB.putFixed64
  decode = first cs . PB.runParser PB.getFixed64

instance HasCodec Int64 where
  encode = PB.runBuilder . PB.putFixed64 . PB.signedInt64ToWord
  decode = first cs . PB.runParser (PB.wordToSignedInt64 <$> PB.getFixed64)

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
