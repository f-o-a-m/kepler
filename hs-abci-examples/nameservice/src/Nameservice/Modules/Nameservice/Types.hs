module Nameservice.Modules.Nameservice.Types where

import           Control.Lens              (iso)
import           Data.Bifunctor            (bimap)
import qualified Data.Binary               as Binary
import           Data.ByteString           (ByteString)
import           Data.ByteString           as BS
import           Data.Int                  (Int32)
import           Data.Maybe                (fromJust)
import           Data.String.Conversions   (cs)
import           Data.Text                 (Text)
import           GHC.Generics              (Generic)
import           Nameservice.Modules.Token (Address, Amount)
import           Tendermint.SDK.Codec      (HasCodec (..))
import           Tendermint.SDK.Errors     (AppError (..), IsAppError (..))
import           Tendermint.SDK.Events     (Event, IsEvent (..), emit)
import qualified Tendermint.SDK.Router     as R
import qualified Tendermint.SDK.Store      as Store


newtype Name = Name String deriving (Eq, Show, Binary.Binary)

nameserviceKey :: ByteString
nameserviceKey = "02"

data Whois = Whois
  { whoisValue :: String
  , whoisOwner :: Address
  , whoisPrice :: Amount
  } deriving (Eq, Show, Generic)

instance Binary.Binary Whois

instance HasCodec Whois where
  encode = cs . Binary.encode
  decode = Right . Binary.decode . cs

instance Store.HasKey Whois where
    type Key Whois = Name
    rawKey = iso (\(Name n) -> nameserviceKey <> cs n)
      (Name . cs . fromJust . BS.stripPrefix nameserviceKey)

instance R.Queryable Whois where
  type Name Whois = "whois"

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data NameserviceException =
    InsufficientBid Text
  | UnauthorizedSet Text

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

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data NameClaimed = NameClaimed
  { nameClaimedOwner :: Address
  , nameClaimedName  :: Name
  , nameClaimedValue :: String
  , nameClaimedBid   :: Amount
  }

instance IsEvent NameClaimed where
  makeEventType _ = "NameClaimed"
  makeEventData NameClaimed{..} = bimap cs cs <$>
    [ (Binary.encode @String "owner", Binary.encode nameClaimedOwner)
    , (Binary.encode @String "name", Binary.encode nameClaimedName)
    , (Binary.encode @String "value", Binary.encode nameClaimedValue)
    , (Binary.encode @String "bid", Binary.encode nameClaimedBid)
    ]

data NameRemapped = NameRemapped
  { nameRemappedName     :: Name
  , nameRemappedOldValue :: String
  , nameRemappedNewValue :: String
  }

instance IsEvent NameRemapped where
  makeEventType _ = "NameRemapped"
  makeEventData NameRemapped{..} = bimap cs cs <$>
    [ (Binary.encode @String "name", Binary.encode nameRemappedName)
    , (Binary.encode @String "oldValue", Binary.encode nameRemappedOldValue)
    , (Binary.encode @String "newValue", Binary.encode nameRemappedNewValue)
    ]

data NameDeleted = NameDeleted
  { nameDeletedName :: Name
  }

instance IsEvent NameDeleted where
  makeEventType _ = "NameDeleted"
  makeEventData NameDeleted{..} = bimap cs cs <$>
    [ (Binary.encode @String "name", Binary.encode nameDeletedName)
    ]
