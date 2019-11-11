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
                                              mkAmount, transfer)
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
  , whoisPrice :: Int32
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
-- Messages
--------------------------------------------------------------------------------

data MsgSetName = MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  }

data MsgBuyName = MsgBuyName
  { msgBuyNameName  :: Name
  , msgBuyNameBig   :: Amount
  , msgBuyNameBuyer :: Address
  }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data OwnerChanged = OwnerChanged
  { ownerChangedNewOwner :: Address
  , ownerChangedName     :: Name
  , ownerChangedNewValue :: String
  , ownerChangedNewPrice :: Int32
  }

instance IsEvent OwnerChanged where
  makeEventType _ = "OwnerChanged"
  makeEventData OwnerChanged{..} = bimap cs cs <$>
    [ (Binary.encode @String "newOwner", Binary.encode ownerChangedNewOwner)
    , (Binary.encode @String "name", Binary.encode ownerChangedName)
    , (Binary.encode @String "newValue", Binary.encode ownerChangedNewValue)
    , (Binary.encode @String "newPrice", Binary.encode ownerChangedNewPrice)
    ]

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data NameserviceException =
  InvalidPurchaseAttempt Text

instance IsAppError NameserviceException where
  makeAppError (InvalidPurchaseAttempt msg) =
    AppError
      { appErrorCode = 1
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
-- | Server
--------------------------------------------------------------------------------

type NameserviceContents = '[Whois]

type Api = "nameservice" :> QueryApi NameserviceContents

server :: Member Store.RawStore r => R.RouteT Api (Sem r)
server = storeQueryHandlers (Proxy :: Proxy NameserviceContents) (Proxy :: Proxy (Sem r))

--------------------------------------------------------------------------------

nameIsAvailable
  :: Member Nameservice r
  => Name
  -> Sem r Bool
nameIsAvailable = fmap isNothing . getWhois

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
    Nothing -> putWhois name whois
    Just Whois{ whoisPrice = forsalePrice
              , whoisOwner = previousOwner
              } ->
      if offerPrice > forsalePrice
        then do
          transfer (whoisOwner whois) (mkAmount offerPrice) previousOwner
          putWhois name whois
          emit $ OwnerChanged
            { ownerChangedNewOwner = whoisOwner whois
            , ownerChangedName = name
            , ownerChangedNewValue = whoisValue whois
            , ownerChangedNewPrice = whoisPrice whois
            }
        else throw (InvalidPurchaseAttempt "Offer too low")
