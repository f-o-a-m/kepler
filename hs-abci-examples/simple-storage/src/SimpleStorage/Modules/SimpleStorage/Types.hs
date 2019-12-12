module SimpleStorage.Modules.SimpleStorage.Types where

import           Control.Lens            (iso)
import           Crypto.Hash             (SHA256 (..), hashWith)
import qualified Data.Aeson              as A
import           Data.Bifunctor          (first)
import           Data.ByteArray          (convert)
import           Data.ByteString         (ByteString)
import           Data.Int                (Int32)
import           Data.String.Conversions (cs)
import           GHC.Generics            (Generic)
import qualified Tendermint.SDK.BaseApp  as BaseApp
import           Tendermint.SDK.Codec    (HasCodec (..))

newtype Count = Count Int32 deriving (Eq, Show, A.ToJSON, A.FromJSON, Serialize.Serialize)

data CountKey = CountKey

instance HasCodec Count where
    encode = Serialize.encode
    decode = first cs . Serialize.decode

instance BaseApp.RawKey CountKey where
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance BaseApp.IsKey CountKey "simple_storage" where
    type Value CountKey "simple_storage" = Count

instance BaseApp.FromQueryData CountKey

instance BaseApp.Queryable Count where
  type Name Count = "count"

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data CountSet = CountSet { newCount :: Count } deriving Generic

countSetOptions :: A.Options
countSetOptions = A.defaultOptions

instance A.ToJSON CountSet where
  toJSON = A.genericToJSON countSetOptions

instance A.FromJSON CountSet where
  parseJSON = A.genericParseJSON countSetOptions

instance BaseApp.ToEvent CountSet where
  makeEventType _ = "count_set"
