{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nameservice.Modules.Nameservice.Types where

import           Control.Lens                 (iso)
import           Data.Aeson                   as A
import           Data.Bifunctor               (bimap)
import           Data.String                  (IsString (..))
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import qualified Data.Text.Lazy               as TL
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           Nameservice.Aeson            (defaultNameserviceOptions)
import           Proto3.Suite                 (HasDefault, Message,
                                               MessageField, Named,
                                               Primitive (..), fromByteString,
                                               toLazyByteString)
import qualified Proto3.Suite.DotProto        as DotProto
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import qualified Tendermint.SDK.BaseApp       as BaseApp
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Modules.Auth  (Amount (..), CoinId (..))
import           Tendermint.SDK.Modules.Bank  ()
import           Tendermint.SDK.Types.Address (Address)

--------------------------------------------------------------------------------

type NameserviceName = "nameservice"

data NameserviceNamespace

--------------------------------------------------------------------------------

newtype Name = Name Text deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON, HasCodec)

instance Primitive Name where
  encodePrimitive n (Name txt) = Encode.text n . TL.fromStrict $ txt
  decodePrimitive = Name . TL.toStrict <$> Decode.text
  primType _ = DotProto.String
instance HasDefault Name
instance MessageField Name
instance IsString Name where
  fromString = Name . fromString

instance BaseApp.FromQueryData Name

data Whois = Whois
  { whoisValue :: Text
  , whoisOwner :: Address
  , whoisPrice :: Amount
  } deriving (Eq, Show)

data WhoisMessage = WhoisMessage
  { whoisMessageValue :: Text
  , whoisMessageOwner :: Address
  , whoisMessagePrice :: Word64
  } deriving (Eq, Show, Generic)
instance Message WhoisMessage
instance Named WhoisMessage

instance HasCodec Whois where
  encode Whois {..} =
    let whoisMessage = WhoisMessage
          { whoisMessageValue = whoisValue
          , whoisMessageOwner = whoisOwner
          , whoisMessagePrice = unAmount whoisPrice
          }
    in cs . toLazyByteString $ whoisMessage
  decode =
    let toWhois WhoisMessage {..} = Whois
          { whoisValue = whoisMessageValue
          , whoisOwner = whoisMessageOwner
          , whoisPrice = Amount whoisMessagePrice
          }
    in bimap (cs . show) toWhois . fromByteString @WhoisMessage

instance BaseApp.RawKey Name where
    rawKey = iso (\(Name n) -> cs n) (Name . cs)

instance BaseApp.IsKey Name NameserviceNamespace where
  type Value Name NameserviceNamespace = Whois

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

data Faucetted = Faucetted
  { faucettedAccount :: Address
  , faucettedCoinId  :: CoinId
  , faucettedAmount  :: Amount
  } deriving (Eq, Show, Generic)

faucettedAesonOptions :: A.Options
faucettedAesonOptions = defaultNameserviceOptions "faucetted"

instance ToJSON Faucetted where
  toJSON = A.genericToJSON faucettedAesonOptions
instance FromJSON Faucetted where
  parseJSON = A.genericParseJSON faucettedAesonOptions
instance BaseApp.ToEvent Faucetted where
  makeEventType _ = "Faucetted"
instance BaseApp.Select Faucetted

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
instance BaseApp.ToEvent NameClaimed
instance BaseApp.Select NameClaimed

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
instance BaseApp.ToEvent NameRemapped
instance BaseApp.Select NameRemapped

data NameDeleted = NameDeleted
  { nameDeletedName :: Name
  } deriving (Eq, Show, Generic)

nameDeletedAesonOptions :: A.Options
nameDeletedAesonOptions = defaultNameserviceOptions "nameDeleted"

instance ToJSON NameDeleted where
  toJSON = A.genericToJSON nameDeletedAesonOptions
instance FromJSON NameDeleted where
  parseJSON = A.genericParseJSON nameDeletedAesonOptions
instance BaseApp.ToEvent NameDeleted
instance BaseApp.Select NameDeleted
