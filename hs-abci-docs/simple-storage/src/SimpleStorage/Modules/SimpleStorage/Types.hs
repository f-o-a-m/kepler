module SimpleStorage.Modules.SimpleStorage.Types where

import qualified Data.Aeson              as A
import           Data.Bifunctor          (first)
import           Data.Int                (Int32)
import qualified Data.Serialize          as Serialize
import qualified Data.Serialize.Text     ()
import           Data.String.Conversions (cs)
import           GHC.Generics            (Generic)
import qualified Tendermint.SDK.BaseApp  as BaseApp
import           Tendermint.SDK.Codec    (HasCodec (..))

type SimpleStorageName = "simple_storage"

newtype Count = Count Int32 deriving (Eq, Show, A.ToJSON, A.FromJSON, Serialize.Serialize)

instance HasCodec Count where
    encode = Serialize.encode
    decode = first cs . Serialize.decode

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

instance BaseApp.ToEvent CountSet

instance BaseApp.Select CountSet

