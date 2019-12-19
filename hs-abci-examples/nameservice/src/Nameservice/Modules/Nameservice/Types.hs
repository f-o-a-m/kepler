module Nameservice.Modules.Nameservice.Types where

import           Control.Lens                 (iso)
import           Data.Aeson                   as A
import           Data.Bifunctor               (first)
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
import qualified Tendermint.SDK.BaseApp       as BaseApp
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Types.Address (Address)

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

instance BaseApp.FromQueryData Name

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

instance BaseApp.RawKey Name where
    rawKey = iso (\(Name n) -> cs n) (Name . cs)

instance BaseApp.IsKey Name NameserviceModule where
  type Value Name NameserviceModule = Whois

instance BaseApp.Queryable Whois where
  type Name Whois = "whois"

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data NameserviceError =
    InsufficientBid Text
  | UnauthorizedSet Text
  | InvalidDelete Text

instance BaseApp.IsAppError NameserviceError where
  makeAppError (InsufficientBid msg) =
    BaseApp.AppError
      { appErrorCode = 1
      , appErrorCodespace = "nameservice"
      , appErrorMessage = msg
      }
  makeAppError (UnauthorizedSet msg) =
    BaseApp.AppError
      { appErrorCode = 2
      , appErrorCodespace = "nameservice"
      , appErrorMessage = msg
      }
  makeAppError (InvalidDelete msg) =
    BaseApp.AppError
      { appErrorCode = 3
      , appErrorCodespace = "nameservice"
      , appErrorMessage = msg
      }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data NameClaimed = NameClaimed
  { nameClaimedOwner :: Address
  , nameClaimedName  :: Name
  , nameClaimedValue :: Text
  , nameClaimedBid   :: Amount
  } deriving (Eq, Show, Generic)

nameClaimedAesonOptions :: A.Options
nameClaimedAesonOptions = defaultNameserviceOptions "nameClaimed"

instance ToJSON NameClaimed where
  toJSON = A.genericToJSON nameClaimedAesonOptions
instance FromJSON NameClaimed where
  parseJSON = A.genericParseJSON nameClaimedAesonOptions
instance BaseApp.ToEvent NameClaimed where
  makeEventType _ = "NameClaimed"
instance BaseApp.FromEvent NameClaimed
instance BaseApp.Select NameClaimed where
  select _ _ = BaseApp.All

data NameRemapped = NameRemapped
  { nameRemappedName     :: Name
  , nameRemappedOldValue :: Text
  , nameRemappedNewValue :: Text
  } deriving (Eq, Show, Generic)

nameRemappedAesonOptions :: A.Options
nameRemappedAesonOptions = defaultNameserviceOptions "nameRemapped"

instance ToJSON NameRemapped where
  toJSON = A.genericToJSON nameRemappedAesonOptions
instance FromJSON NameRemapped where
  parseJSON = A.genericParseJSON nameRemappedAesonOptions
instance BaseApp.ToEvent NameRemapped where
  makeEventType _ = "NameRemapped"
instance BaseApp.FromEvent NameRemapped
instance BaseApp.Select NameRemapped where
  select _ _ = BaseApp.All

data NameDeleted = NameDeleted
  { nameDeletedName :: Name
  } deriving (Eq, Show, Generic)

nameDeletedAesonOptions :: A.Options
nameDeletedAesonOptions = defaultNameserviceOptions "nameDeleted"

instance ToJSON NameDeleted where
  toJSON = A.genericToJSON nameDeletedAesonOptions
instance FromJSON NameDeleted where
  parseJSON = A.genericParseJSON nameDeletedAesonOptions
instance BaseApp.ToEvent NameDeleted where
  makeEventType _ = "NameDeleted"
instance BaseApp.FromEvent NameDeleted
instance BaseApp.Select NameDeleted where
  select _ _ = BaseApp.All
