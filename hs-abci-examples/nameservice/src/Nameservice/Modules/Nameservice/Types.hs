module Nameservice.Modules.Nameservice.Types where

import           Control.Lens                 (iso, (&), (.~), (^.))
import           Control.Lens.Wrapped         (Wrapped (..))
import           Data.Aeson                   as A
import           Data.Bifunctor               (first)
import           Data.ByteString              (ByteString)
import           Data.ByteString              as BS
import           Data.Maybe                   (fromJust)
import qualified Data.Serialize               as Serialize
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as TL
import           GHC.Generics                 (Generic)
import           Nameservice.Aeson            (defaultNameserviceOptions)
import           Nameservice.Modules.Token    (Amount (..))
import           Proto3.Suite                 (HasDefault, Message,
                                               MessageField, Named,
                                               Primitive (..), fromByteString,
                                               toLazyByteString)
import qualified Proto3.Suite.DotProto        as DotProto
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Errors        (AppError (..), IsAppError (..))
import           Tendermint.SDK.Events        (FromEvent (..), ToEvent (..))
import qualified Tendermint.SDK.Query         as Q
import qualified Tendermint.SDK.Store         as Store
import           Tendermint.SDK.Types.Address (Address, addressFromBytes,
                                               addressToBytes)

--------------------------------------------------------------------------------

type NameserviceModule = "nameservice"

--------------------------------------------------------------------------------

newtype Name = Name Text deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON)
instance Primitive Name where
  encodePrimitive n (Name txt) = Encode.text n . TL.fromStrict $ txt
  decodePrimitive = Name . TL.toStrict <$> Decode.text
  primType _ = DotProto.String
instance HasDefault Name
instance MessageField Name

instance Q.FromQueryData Name

data Whois = Whois
  { whoisValue :: Text
  , whoisOwner :: Address
  , whoisPrice :: Amount
  } deriving (Eq, Show, Generic)
instance Message Whois
instance Named Whois

instance HasCodec Whois where
  encode = cs . toLazyByteString
  decode = first (cs . show) . fromByteString

instance Store.RawKey Name where
    rawKey = iso (\(Name n) -> cs n) (Name . cs)

instance Store.IsKey Name NameserviceModule where
    type Value Name NameserviceModule = Whois

instance Q.Queryable Whois where
  type Name Whois = "whois"

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data NameserviceException =
    InsufficientBid Text
  | UnauthorizedSet Text
  | InvalidDelete Text

instance IsAppError NameserviceException where
  makeAppError (InsufficientBid msg) =
    AppError
      { appErrorCode = 1
      , appErrorCodespace = "nameservice"
      , appErrorMessage = msg
      }
  makeAppError (UnauthorizedSet msg) =
    AppError
      { appErrorCode = 2
      , appErrorCodespace = "nameservice"
      , appErrorMessage = msg
      }
  makeAppError (InvalidDelete msg) =
    AppError
      { appErrorCode = 3
      , appErrorCodespace = "nameservice"
      , appErrorMessage = msg
      }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data Faucetted = Faucetted
  { faucettedAccount :: Address
  , faucettedAmount  :: Amount
  } deriving (Generic)

faucettedAesonOptions :: A.Options
faucettedAesonOptions = defaultNameserviceOptions "faucetted"

instance ToJSON Faucetted where
  toJSON = A.genericToJSON faucettedAesonOptions
instance FromJSON Faucetted where
  parseJSON = A.genericParseJSON faucettedAesonOptions
instance ToEvent Faucetted where
  makeEventType _ = "Faucetted"

data NameClaimed = NameClaimed
  { nameClaimedOwner :: Address
  , nameClaimedName  :: Name
  , nameClaimedValue :: Text
  , nameClaimedBid   :: Amount
  } deriving (Generic)

nameClaimedAesonOptions :: A.Options
nameClaimedAesonOptions = defaultNameserviceOptions "nameClaimed"

instance ToJSON NameClaimed where
  toJSON = A.genericToJSON nameClaimedAesonOptions
instance FromJSON NameClaimed where
  parseJSON = A.genericParseJSON nameClaimedAesonOptions
instance ToEvent NameClaimed where
  makeEventType _ = "NameClaimed"

data NameRemapped = NameRemapped
  { nameRemappedName     :: Name
  , nameRemappedOldValue :: Text
  , nameRemappedNewValue :: Text
  } deriving Generic

nameRemappedAesonOptions :: A.Options
nameRemappedAesonOptions = defaultNameserviceOptions "nameRemapped"

instance ToJSON NameRemapped where
  toJSON = A.genericToJSON nameRemappedAesonOptions
instance FromJSON NameRemapped where
  parseJSON = A.genericParseJSON nameRemappedAesonOptions
instance ToEvent NameRemapped where
  makeEventType _ = "NameRemapped"
instance FromEvent NameRemapped

data NameDeleted = NameDeleted
  { nameDeletedName :: Name
  } deriving Generic

nameDeletedAesonOptions :: A.Options
nameDeletedAesonOptions = defaultNameserviceOptions "nameDeleted"

instance ToJSON NameDeleted where
  toJSON = A.genericToJSON nameDeletedAesonOptions
instance FromJSON NameDeleted where
  parseJSON = A.genericParseJSON nameDeletedAesonOptions
instance ToEvent NameDeleted where
  makeEventType _ = "NameDeleted"
instance FromEvent NameDeleted
