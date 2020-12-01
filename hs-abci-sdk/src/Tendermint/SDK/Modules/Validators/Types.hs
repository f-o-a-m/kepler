module Tendermint.SDK.Modules.Validators.Types where

import qualified Data.Aeson                             as A
import           Data.ByteString                        (ByteString)
import           Data.ByteString.Lazy                   (toStrict)
import           GHC.Generics                           (Generic)
import           Network.ABCI.Types.Messages.FieldTypes (ValidatorUpdate)
import           Tendermint.SDK.Codec                   (HasCodec (..))

data ValidatorsNameSpace

type ValidatorsName = "validators"

heightKey :: ByteString
heightKey = "height"

updatesKey :: ByteString
updatesKey = "updates"



newtype UpdatesList = UpdatesList [ValidatorUpdate] deriving (Eq, Show, Generic)
instance A.ToJSON UpdatesList

instance A.FromJSON UpdatesList

instance HasCodec UpdatesList where
  encode = toStrict . A.encode
  decode s =
    let hs :: Maybe UpdatesList = A.decodeStrict s
     in case hs of
          Just h  -> Right h
          Nothing -> Left "failure to decode UpdatesList"
