{-# LANGUAGE TemplateHaskell #-}

module SimpleStorage.Modules.SimpleStorage
  (
  -- * Component
    SimpleStorage
  , putCount
  , getCount

  , Api
  , server
  , eval
  , initialize

  -- * Store
  , CountStoreContents

  -- * Types
  , Count(..)
  , CountKey(..)

  -- * Events
  , CountSet

  ) where

import           Control.Lens                (iso)
import           Crypto.Hash                 (SHA256 (..), hashWith)
import qualified Data.Binary                 as Binary
import           Data.ByteArray              (convert)
import           Data.ByteString             (ByteString)
import           Data.Int                    (Int32)
import           Data.Maybe                  (fromJust)
import           Data.Proxy
import           Data.String.Conversions     (cs)
import           Polysemy                    (Member, Sem, interpret, makeSem)
import           Polysemy.Output             (Output)
import           Servant.API                 ((:>))
import           Tendermint.SDK.BaseApp      (BaseApp)
import           Tendermint.SDK.Codec        (HasCodec (..))
import qualified Tendermint.SDK.Events       as Events
import           Tendermint.SDK.Router       (EncodeQueryResult, FromQueryData,
                                              Queryable (..), RouteT)
import           Tendermint.SDK.Store        (HasKey (..), RawStore, Root, get,
                                              put)
import           Tendermint.SDK.StoreQueries (QueryApi, storeQueryHandlers)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Count = Count Int32 deriving (Eq, Show)

data CountKey = CountKey

instance HasCodec Count where
    encode (Count c) = cs . Binary.encode $ c
    decode = Right . Count . Binary.decode . cs

instance HasKey Count where
    type Key Count = CountKey
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance FromQueryData CountKey

instance EncodeQueryResult Count

instance Queryable Count where
  type Name Count = "count"

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data CountSet = CountSet { newCount :: Count }

instance Events.IsEvent CountSet where
  makeEventType _ = "count_set"
  makeEventData CountSet{newCount} = [("new_count", encode newCount)]

--------------------------------------------------------------------------------
-- SimpleStorage Module
--------------------------------------------------------------------------------

data SimpleStorage m a where
    PutCount :: Count -> SimpleStorage m ()
    GetCount :: SimpleStorage m Count

makeSem ''SimpleStorage

eval
  :: forall r.
     BaseApp r
  => forall a. (Sem (SimpleStorage ': r) a -> Sem r a)
eval = interpret (\case
  PutCount count -> do
    put CountKey count
    Events.emit $ CountSet count

  GetCount -> fromJust <$> get (undefined :: Root) CountKey
  )

initialize
  :: BaseApp r
  => Member (Output Events.Event) r
  => Sem r ()
initialize = eval $ do
  putCount (Count 0)

--------------------------------------------------------------------------------
-- Query Api
--------------------------------------------------------------------------------

type CountStoreContents = '[Count]

type Api = "simple_storage" :> QueryApi CountStoreContents

server :: Member RawStore r => RouteT Api (Sem r)
server = storeQueryHandlers (Proxy :: Proxy CountStoreContents) (Proxy :: Proxy (Sem r))
