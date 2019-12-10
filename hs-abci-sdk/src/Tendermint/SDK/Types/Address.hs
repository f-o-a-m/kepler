module Tendermint.SDK.Types.Address where

import qualified Crypto.Secp256k1         as Crypto
import qualified Data.Aeson               as A
import qualified Data.ByteArray.HexString as Hex
import           Data.ByteString          (ByteString)
import           GHC.Generics             (Generic)

-- | Used as a unique identifier for an account.
newtype Address =
    Address Hex.HexString
    deriving (Eq, Show, Generic, Ord, A.ToJSON, A.FromJSON)

addressToBytes :: Address -> ByteString
addressToBytes (Address addrHex) = Hex.toBytes addrHex

addressFromBytes :: ByteString -> Address
addressFromBytes = Address . Hex.fromBytes

pubKeyToAddress :: Crypto.PubKey -> Address
pubKeyToAddress = addressFromBytes . Crypto.exportPubKey False
