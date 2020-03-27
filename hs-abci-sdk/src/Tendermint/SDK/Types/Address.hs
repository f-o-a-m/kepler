{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.Types.Address where

import qualified Crypto.Secp256k1         as Crypto
import qualified Data.Aeson               as A
import qualified Data.ByteArray.HexString as Hex
import           Data.ByteString          (ByteString)
import           Data.String              (fromString)
import           Data.Text                (unpack)
import           GHC.Generics             (Generic)
import           Proto3.Suite             (HasDefault (..), MessageField,
                                           Primitive (..))
import qualified Proto3.Suite.DotProto    as DotProto
import qualified Proto3.Wire.Decode       as Decode
import qualified Proto3.Wire.Encode       as Encode
import           Tendermint.SDK.Codec     (HasCodec (..))
import           Web.HttpApiData          (FromHttpApiData (..),
                                           ToHttpApiData (..))

-- | Used as a unique identifier for an account.
newtype Address =
    Address Hex.HexString
    deriving (Eq, Show, Generic, Ord, A.ToJSON, A.FromJSON)

instance Primitive Address where
  encodePrimitive n a = Encode.byteString n $ addressToBytes a
  decodePrimitive = addressFromBytes <$> Decode.byteString
  primType _ = DotProto.Bytes
instance HasDefault Hex.HexString
instance HasDefault Address
instance MessageField Address
instance HasCodec Address where
  decode = Right . addressFromBytes
  encode = addressToBytes
instance ToHttpApiData Address where
  toQueryParam (Address aHex) = Hex.format aHex
instance FromHttpApiData Address where
  parseQueryParam = Right . Address . fromString . unpack

addressToBytes :: Address -> ByteString
addressToBytes (Address addrHex) = Hex.toBytes addrHex

addressFromBytes :: ByteString -> Address
addressFromBytes = Address . Hex.fromBytes

pubKeyToAddress :: Crypto.PubKey -> Address
pubKeyToAddress = addressFromBytes . Crypto.exportPubKey False
