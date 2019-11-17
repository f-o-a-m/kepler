module Tendermint.SDK.Types.Address where

import           Control.Lens             (iso)
import qualified Crypto.Secp256k1         as Crypto
import qualified Data.Aeson               as A
import qualified Data.ByteArray.HexString as Hex
import           Data.ByteString          (ByteString)
import           GHC.Generics             (Generic)
import           Tendermint.SDK.Query     (FromQueryData)
import           Tendermint.SDK.Store     (RawKey (..))

-- | Used as a unique identifier for an account.
newtype Address =
    Address Hex.HexString
    deriving (Eq, Show, Generic, Ord, A.ToJSON, A.FromJSON)

instance RawKey Address where
    rawKey = iso (\(Address a) -> Hex.toBytes a) (Address . Hex.fromBytes)

instance FromQueryData Address

addressToBytes :: Address -> ByteString
addressToBytes (Address addrHex) = Hex.toBytes addrHex

addressFromBytes :: ByteString -> Address
addressFromBytes = Address . Hex.fromBytes

pubKeyToAddress :: Crypto.PubKey -> Address
pubKeyToAddress = addressFromBytes . Crypto.exportPubKey False
