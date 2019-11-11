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
                                              transfer)
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
  { nameClaimedNewOwner :: Address
  , nameClaimedName     :: Name
  , nameClaimedNewValue :: String
  , nameClaimedNewPrice :: Amount
  }

instance IsEvent NameClaimed where
  makeEventType _ = "NameClaimed"
  makeEventData NameClaimed{..} = bimap cs cs <$>
    [ (Binary.encode @String "newOwner", Binary.encode nameClaimedNewOwner)
    , (Binary.encode @String "name", Binary.encode nameClaimedName)
    , (Binary.encode @String "newValue", Binary.encode nameClaimedNewValue)
    , (Binary.encode @String "newPrice", Binary.encode nameClaimedNewPrice)
    ]

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data NameserviceException =
    InvalidPurchaseAttempt Text
  | InvalidSetAttempt Text

instance IsAppError NameserviceException where
  makeAppError (InvalidPurchaseAttempt msg) =
    AppError
      { appErrorCode = 1
      , appErrorCodespace = "nameservice"
      , appErrorMessage = msg
      }
  makeAppError (InvalidSetAttempt msg) =
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

data NameserviceMessage =
    MsgSetName
      { msgSetNameName  :: Name
      , msgSetNameValue :: String
      , msgSetNameOwner :: Address
      , msgSetNamePrice :: Amount
      }
  | MsgBuyName
    { msgBuyNameName  :: Name
    , msgBuyNameValue :: String
    , msgBuyNameBuyer :: Address
    , msgBuyNamePrice :: Amount
    }

router
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => NameserviceMessage
  -> Sem r ()
router = \case
  MsgSetName {..} ->
    let whois = Whois
          { whoisOwner = msgSetNameOwner
          , whoisValue = msgSetNameValue
          , whoisPrice = msgSetNamePrice
          }
    in setName msgSetNameName whois
  MsgBuyName{..} ->
    let whois = Whois
          { whoisOwner = msgBuyNameBuyer
          , whoisValue = msgBuyNameValue
          , whoisPrice = msgBuyNamePrice
          }
    in buyName msgBuyNameName whois


nameIsAvailable
  :: Member Nameservice r
  => Name
  -> Sem r Bool
nameIsAvailable = fmap isNothing . getWhois

setName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => Name
  -> Whois
  -> Sem r ()
setName name whois = do
  isAvailable <- nameIsAvailable name
  if isAvailable
    then do
      emit NameClaimed
        { nameClaimedName = name
        , nameClaimedNewOwner = whoisOwner whois
        , nameClaimedNewValue = whoisValue whois
        , nameClaimedNewPrice = whoisPrice whois
        }
      putWhois name whois
    else throw $ InvalidSetAttempt "Name is not available."

buyName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => Name
  -> Whois
  -> Sem r ()
-- ^ did it succeed
buyName name whois@Whois{whoisPrice=offerPrice} = do
  mWhois <- getWhois name
  case mWhois of
    Nothing -> setName name whois
    Just Whois{ whoisPrice = forsalePrice
              , whoisOwner = previousOwner
              } ->
      if offerPrice > forsalePrice
        then do
          transfer (whoisOwner whois) offerPrice previousOwner
          putWhois name whois
          emit NameClaimed
            { nameClaimedNewOwner = whoisOwner whois
            , nameClaimedName = name
            , nameClaimedNewValue = whoisValue whois
            , nameClaimedNewPrice = whoisPrice whois
            }
        else throw (InvalidPurchaseAttempt "Offer too low")
