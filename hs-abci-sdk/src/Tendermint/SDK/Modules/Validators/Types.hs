module Tendermint.SDK.Modules.Validators.Types where

import           Control.Lens                           (iso)
import qualified Data.Aeson                             as A
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Lazy                   (toStrict)
import           Data.Maybe                             (fromJust)
import           Data.Set                               (Set)
import           Data.Word                              (Word64)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.FieldTypes (PubKey)
import           Tendermint.SDK.BaseApp                 (RawKey (..))
import           Tendermint.SDK.Codec                   (HasCodec (..))


data ValidatorsNameSpace

type ValidatorsName = "validators"


updatesListKey :: ByteString
updatesListKey = "updatesList"

validatorsMapKey :: ByteString
validatorsMapKey = "validatorsMap"

validatorsKeySetKey :: ByteString
validatorsKeySetKey = "validatorsKeySet"


data ValidatorUpdate = ValidatorUpdate
    { key   :: PubKey_
    , power :: Word64
    }
    deriving Generic
instance A.ToJSON ValidatorUpdate
instance A.FromJSON ValidatorUpdate
instance HasCodec ValidatorUpdate where
  encode = toStrict . A.encode
  decode s = maybe (Left "failure to decode ValidatorUpdate") Right (A.decodeStrict s)


newtype PubKey_ = PubKey_ PubKey deriving (Eq, Ord, Generic)
instance A.ToJSON PubKey_
instance A.FromJSON PubKey_
instance RawKey PubKey_ where
  rawKey = iso (\(PubKey_ p) -> (toStrict . A.encode) p) (PubKey_ . fromJust . A.decodeStrict)


newtype KeySet = KeySet (Set PubKey_) deriving Generic
instance A.ToJSON KeySet
instance A.FromJSON KeySet
instance HasCodec KeySet where
  encode = toStrict . A.encode
  decode s = maybe (Left "failure to decode KeySet") Right (A.decodeStrict s)
