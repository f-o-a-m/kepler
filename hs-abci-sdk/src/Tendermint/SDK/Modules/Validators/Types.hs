module Tendermint.SDK.Modules.Validators.Types where

import           Control.Lens                           (Wrapped (_Wrapped'),
                                                         iso, (^.), _Unwrapped')
import qualified Data.Aeson                             as A
import           Data.Bifunctor                         (Bifunctor (bimap, second))
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Lazy                   (toStrict)
import           Data.Either                            (fromRight)
import           Data.Map                               (Map)
import           Data.ProtoLens                         (decodeMessage,
                                                         encodeMessage)
import           Data.Set                               (Set)
import           Data.String.Conversions                (cs)
import           Data.Word                              (Word64)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.FieldTypes (PubKey (PubKey),
                                                         ValidatorUpdate)
import           Tendermint.SDK.BaseApp                 (QueryData, RawKey (..))
import           Tendermint.SDK.Codec                   (HasCodec (..))


data ValidatorsNameSpace

type ValidatorsName = "validators"


updatesListKey :: ByteString
updatesListKey = "updatesList"

validatorsMapKey :: ByteString
validatorsMapKey = "validatorsMap"

validatorsKeySetKey :: ByteString
validatorsKeySetKey = "validatorsKeySet"


newtype ValidatorUpdate_ = ValidatorUpdate_ ValidatorUpdate deriving (Eq, Generic)

instance HasCodec ValidatorUpdate_ where
  encode (ValidatorUpdate_ vu) = encodeMessage $ (vu ^. _Wrapped')
  decode bs = bimap cs (ValidatorUpdate_ . (^. _Unwrapped')) $ decodeMessage bs

newtype PubKey_ = PubKey_ PubKey deriving (Eq, Ord, Generic)

instance RawKey PubKey_ where
  rawKey = iso t f
    where
      t (PubKey_ p) = encodeMessage $ (p ^. _Wrapped')
      f = PubKey_ . fromRight (PubKey "" "") . second (^. _Unwrapped') . decodeMessage


instance A.ToJSON PubKey_
instance A.ToJSONKey PubKey_
instance A.FromJSON PubKey_
instance A.FromJSONKey PubKey_

instance HasCodec (Map PubKey_ Word64) where
  encode = toStrict . A.encode
  decode s =
    let ms :: Maybe (Map PubKey_ Word64) = A.decodeStrict s
     in case ms of
          Just m  -> Right m
          Nothing -> Left "failure to decode Map of Validators"

instance QueryData PubKey_

newtype KeySet = KeySet (Set PubKey_) deriving Generic
instance A.ToJSON KeySet
instance A.FromJSON KeySet
instance HasCodec KeySet where
  encode = toStrict . A.encode
  decode s = maybe (Left "failure to decode KeySet") Right (A.decodeStrict s)
