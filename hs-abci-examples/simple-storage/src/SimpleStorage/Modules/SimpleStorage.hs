module SimpleStorage.Modules.SimpleStorage
  (
  -- * Component
    SimpleStorageEffR
  , HasSimpleStorageEff

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
import qualified Data.Aeson                  as A
import qualified Data.Binary                 as Binary
import           Data.ByteArray              (convert)
import           Data.ByteString             (ByteString)
import           Data.Int                    (Int32)
import           Data.Proxy
import           Data.String.Conversions     (cs)
import           GHC.Generics                (Generic)
import           Polysemy                    (Member, Members, Sem)
import           Polysemy.Output             (Output)
import           Servant.API                 ((:>))
import           Tendermint.SDK.BaseApp      (HasBaseApp)
import           Tendermint.SDK.Codec        (HasCodec (..))
import qualified Tendermint.SDK.Events       as Events
import           Tendermint.SDK.Router       (EncodeQueryResult, FromQueryData,
                                              Queryable (..), RouteT)
import           Tendermint.SDK.Store        (IsKey (..), IsRawKey (..), Store,
                                              put)
import qualified Tendermint.SDK.Store        as Store
import           Tendermint.SDK.StoreQueries (QueryApi, storeQueryHandlers)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Count = Count Int32 deriving (Eq, Show, A.ToJSON, A.FromJSON)

data CountKey = CountKey

instance HasCodec Count where
    encode (Count c) = cs . Binary.encode $ c
    decode = Right . Count . Binary.decode . cs

instance IsRawKey CountKey where
  rawKey = iso (\_ -> cs countKey) (const CountKey)
    where
      countKey :: ByteString
      countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance IsKey "simple_storage" CountKey where
    type Value "simple_storage" CountKey = Count

instance FromQueryData CountKey

instance EncodeQueryResult Count

instance Queryable Count where
  type Name Count = "count"

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data CountSet = CountSet { newCount :: Count } deriving Generic

countSetOptions :: A.Options
countSetOptions = A.defaultOptions

instance A.ToJSON CountSet where
  toJSON = A.genericToJSON countSetOptions

instance A.FromJSON CountSet where
  parseJSON = A.genericParseJSON countSetOptions

instance Events.ToEvent CountSet where
  makeEventType _ = "count_set"

--------------------------------------------------------------------------------
-- SimpleStorage Module
--------------------------------------------------------------------------------

type SimpleStorageEffR = '[Store "simple_storage"]

type HasSimpleStorageEff r =
  (Members SimpleStorageEffR r, Member (Output Events.Event) r)

eval
  :: HasBaseApp r
  => Sem (Store "simple_storage" ': r) a
  -> Sem r a
eval = Store.eval

initialize
  :: HasBaseApp r
  => Member (Output Events.Event) r
  => Sem r ()
initialize = eval $ do
  put CountKey (Count 0)

--------------------------------------------------------------------------------
-- Query Api
--------------------------------------------------------------------------------

type CountStoreContents = '[(CountKey, Count)]

type Api = "simple_storage" :> QueryApi CountStoreContents

server :: Member (Store "simple_storage") r => RouteT Api (Sem r)
server = storeQueryHandlers (Proxy :: Proxy CountStoreContents) (Proxy :: Proxy (Sem r))
