{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Nameservice

  (
    -- * types
    Name(..)
  , Whois (..)
  , NameserviceException(..)

  -- * effects
  , NameserviceEffR
  , HasNameserviceEff
  , getWhois
  , nameIsAvailable
  , buyName

  -- * interpreter
  , eval

  -- * query API
  , Api
  , server

  ) where

import           Control.Lens                (iso)
import           Data.Bifunctor              (bimap)
import qualified Data.Binary                 as Binary
import           Data.ByteString             (ByteString)
import           Data.ByteString             as BS
import           Data.Int                    (Int32)
import           Data.Maybe                  (fromJust, isNothing)
import           Data.Proxy
import           Data.String.Conversions     (cs)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Nameservice.Modules.Token   (Address, Amount, HasTokenEff,
                                              burn, mint, transfer)
import           Polysemy                    (Member, Members, Sem, interpret,
                                              makeSem)
import           Polysemy.Error              (Error, mapError, throw)
import           Polysemy.Output             (Output)
import           Servant.API                 ((:>))
import           Tendermint.SDK.BaseApp      (HasBaseApp)
import           Tendermint.SDK.Codec        (HasCodec (..))
import           Tendermint.SDK.Errors       (AppError (..), IsAppError (..))
import           Tendermint.SDK.Events       (Event, IsEvent (..), emit)
import qualified Tendermint.SDK.Router       as R
import qualified Tendermint.SDK.Store        as Store
import           Tendermint.SDK.StoreQueries (QueryApi, storeQueryHandlers)

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

data Nameservice m a where
  PutWhois :: Name -> Whois -> Nameservice m ()
  GetWhois :: Name -> Nameservice m (Maybe Whois)

makeSem ''Nameservice

type NameserviceEffR = '[Nameservice, Error NameserviceException]
type HasNameserviceEff r = Members NameserviceEffR r

eval
  :: HasBaseApp r
  => HasTokenEff r
  => Member (Error AppError) r
  => Sem (Nameservice ': Error NameserviceException ': r) a
  -> Sem r a
eval = mapError makeAppError . evalNameservice
  where
    evalNameservice
      :: HasBaseApp r
      => Sem (Nameservice ': r) a
      -> Sem r a
    evalNameservice =
      interpret (\case
          GetWhois name ->
            Store.get (undefined :: Store.Root) name
          PutWhois name whois ->
            Store.put name whois
        )

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type NameserviceContents = '[Whois]

type Api = "nameservice" :> QueryApi NameserviceContents

server :: Member Store.RawStore r => R.RouteT Api (Sem r)
server = storeQueryHandlers (Proxy :: Proxy NameserviceContents) (Proxy :: Proxy (Sem r))

--------------------------------------------------------------------------------
--  Message Handlers
--------------------------------------------------------------------------------

data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  }

data MsgDeletename = MsgDeleteName
  { msgDeleteNameName  :: Name
  , msgDeleteNameOwner :: Address
  }

data MsgBuyName = MsgBuyName
    { msgBuyNameName  :: Name
    , msgBuyNameValue :: String
    , msgBuyNameBuyer :: Address
    , msgBuyNameBid   :: Amount
    }

--router
--  :: HasTokenEff r
--  => HasNameserviceEff r
--  => Member (Output Event) r
--  => NameserviceMessage
--  -> Sem r ()
--router = \case
--  MsgSetName {..} ->
--    let whois = Whois
--          { whoisOwner = msgSetNameOwner
--          , whoisValue = msgSetNameValue
--          }
--    in setName msgSetNameName whois
--  MsgBuyName{..} ->
--    let whois = Whois
--          { whoisOwner = msgBuyNameBuyer
--          , whoisValue = msgBuyNameValue
--          , whoisPrice = msgBuyNamePrice
--          }
--    in buyName msgBuyNameName whois

--------------------------------------------------------------------------------

nameIsAvailable
  :: Member Nameservice r
  => Name
  -> Sem r Bool
nameIsAvailable = fmap isNothing . getWhois

setName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => MsgSetName
  -> Sem r ()
setName MsgSetName{..} = do
  mwhois <- getWhois msgSetNameName
  case mwhois of
    Nothing -> throw $ UnauthorizedSet "Cannot claim name with SetMessage tx."
    Just currentWhois@Whois{..} ->
      if whoisOwner /= msgSetNameOwner
        then throw $ UnauthorizedSet "Setter must be the owner of the Name."
        else do
          putWhois msgSetNameName currentWhois {whoisValue = msgSetNameValue}
          emit NameRemapped
             { nameRemappedName = msgSetNameName
             , nameRemappedNewValue = msgSetNameValue
             , nameRemappedOldValue = whoisValue
             }

buyUnclaimedName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => MsgBuyName
  -> Sem r ()
buyUnclaimedName MsgBuyName{..} = do
  burn msgBuyNameBuyer msgBuyNameBid
  let whois = Whois
        { whoisOwner = msgBuyNameBuyer
        , whoisValue = msgBuyNameValue
        , whoisPrice = msgBuyNameBid
        }
  putWhois msgBuyNameName whois
  emit NameClaimed
    { nameClaimedOwner = msgBuyNameBuyer
    , nameClaimedName = msgBuyNameName
    , nameClaimedValue = msgBuyNameValue
    , nameClaimedBid = msgBuyNameBid
    }

buyName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => MsgBuyName
  -> Sem r ()
-- ^ did it succeed
buyName msg@MsgBuyName{..} = do
  let name =  msgBuyNameName
  mWhois <- getWhois name
  case mWhois of
    -- The name is unclaimed, go ahead and debit the account
    -- and create it.
    Nothing -> buyUnclaimedName msg
    -- The name is currently claimed, we will transfer the
    -- funds and ownership
    Just currentWhois@Whois
           { whoisPrice = forsalePrice
           , whoisOwner = previousOwner
           } ->
      if msgBuyNameBid > forsalePrice
        then do
          transfer msgBuyNameBuyer msgBuyNameBid previousOwner
          putWhois name currentWhois {whoisOwner = msgBuyNameBuyer}
          emit NameClaimed
            { nameClaimedOwner = msgBuyNameBuyer
            , nameClaimedName = name
            , nameClaimedValue = msgBuyNameValue
            , nameClaimedBid = msgBuyNameBid
            }
        else throw (InsufficientBid "Bid must exceed the price.")
