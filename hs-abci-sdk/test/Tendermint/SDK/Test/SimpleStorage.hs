{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.SimpleStorage
  ( SimpleStorageM
  , SimpleStorage
  , QueryApi
  , MessageApi
  , UpdateCountTx(..)
  , simpleStorageModule
  , evalToIO
  , Count(..)
  ) where

import           Control.Lens                     (iso, (^.))
import           Crypto.Hash                      (SHA256 (..), hashWith)
import           Data.Bifunctor                   (first)
import           Data.ByteArray                   (convert)
import qualified Data.ByteArray.Base64String      as Base64
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int32)
import           Data.Maybe                       (fromJust)
import           Data.Proxy
import qualified Data.Serialize                   as Serialize
import           Data.Serialize.Text              ()
import qualified Data.Serialize.Text              ()
import           Data.String.Conversions          (cs)
import           Data.Validation                  (Validation (..))
import           GHC.Generics                     (Generic)
import           Polysemy
import           Polysemy.Error                   (Error)
import           Servant.API
import           Tendermint.SDK.Application       (Module (..))
import qualified Tendermint.SDK.BaseApp           as BaseApp
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Types.Message     (Msg (..))
import           Tendermint.SDK.Types.Message     (ValidateMessage (..))
import           Tendermint.SDK.Types.Transaction (Tx (..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Count = Count Int32 deriving (Eq, Show, Ord, Num, Serialize.Serialize)

data CountKey = CountKey

instance HasCodec Count where
    encode = Serialize.encode
    decode = first cs . Serialize.decode

instance BaseApp.RawKey CountKey where
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance BaseApp.IsKey CountKey "simple_storage" where
    type Value CountKey "simple_storage" = Count

instance BaseApp.FromQueryData CountKey

instance BaseApp.Queryable Count where
  type Name Count = "count"

--------------------------------------------------------------------------------
-- Message Types
--------------------------------------------------------------------------------

data UpdateCountTx = UpdateCountTx
  { updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

instance Serialize.Serialize UpdateCountTx

instance HasCodec UpdateCountTx where
  encode = Serialize.encode
  decode = first cs . Serialize.decode

instance ValidateMessage UpdateCountTx where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------
-- Keeper
--------------------------------------------------------------------------------

storeKey :: BaseApp.StoreKey "simple_storage"
storeKey = BaseApp.StoreKey "simple_storage"

data SimpleStorage m a where
    PutCount :: Count -> SimpleStorage m ()
    GetCount :: SimpleStorage m Count

makeSem ''SimpleStorage

type SimpleStorageEffs = '[SimpleStorage]

updateCount
  ::  Member SimpleStorage r
  => Count
  -> Sem r ()
updateCount count = putCount count

eval
  :: forall r.
     Members '[BaseApp.RawStore, Error BaseApp.AppError] r
  => forall a. (Sem (SimpleStorage ': r) a -> Sem r a)
eval = interpret (\case
  PutCount count -> BaseApp.put storeKey CountKey count
  GetCount -> fromJust <$> BaseApp.get storeKey CountKey
  )

--------------------------------------------------------------------------------
-- Router
--------------------------------------------------------------------------------

type MessageApi =
  BaseApp.TypedMessage "update_count" UpdateCountTx BaseApp.:~> BaseApp.Return ()

messageHandlers
  :: Member SimpleStorage r
  => BaseApp.RouteTx MessageApi r 'BaseApp.DeliverTx
messageHandlers = updateCountH

updateCountH
  :: Member SimpleStorage r
  => Members BaseApp.TxEffs r
  => BaseApp.RoutingTx UpdateCountTx
  -> Sem r ()
updateCountH (BaseApp.RoutingTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
      UpdateCountTx{updateCountTxCount} = msgData
  in updateCount (Count updateCountTxCount)

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

type CountStoreContents = '[(CountKey, Count)]

type GetMultipliedCount =
     "manipulated"
  :> Capture "subtract" Integer
  :> QueryParam' '[Required, Strict] "factor" Integer
  :> BaseApp.Leaf Count

getMultipliedCount
  :: Member SimpleStorage r
  => Integer
  -> Integer
  -> Sem r (BaseApp.QueryResult Count)
getMultipliedCount subtractor multiplier = do
  let m = fromInteger multiplier
      s = fromInteger subtractor
  c <- getCount
  pure $ BaseApp.QueryResult
    { queryResultData = m * c - s
    , queryResultIndex = 0
    , queryResultKey = Base64.fromBytes $ CountKey ^. BaseApp.rawKey
    , queryResultProof  = Nothing
    , queryResultHeight = 0
    }

type QueryApi = GetMultipliedCount :<|> BaseApp.QueryApi CountStoreContents

server
  :: forall r.
     Members [SimpleStorage, BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteQ QueryApi r
server =
  let storeHandlers = BaseApp.storeQueryHandlers (Proxy :: Proxy CountStoreContents)
        storeKey (Proxy :: Proxy r)
  in getMultipliedCount :<|> storeHandlers

--------------------------------------------------------------------------------
-- Module Definition
--------------------------------------------------------------------------------

type SimpleStorageM r =
  Module "simple_storage" MessageApi QueryApi SimpleStorageEffs r

simpleStorageModule
  :: Member SimpleStorage r
  => Members BaseApp.BaseAppEffs r
  => SimpleStorageM r
simpleStorageModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = BaseApp.defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQueryServer = server
  , moduleEval = eval
  }

evalToIO
  :: BaseApp.Context
  -> Sem (SimpleStorage ': BaseApp.BaseApp BaseApp.CoreEffs) a
  -> IO a
evalToIO context action =
  BaseApp.runCoreEffs context .
    BaseApp.compileToCoreEffs .
    BaseApp.applyScope @'BaseApp.Consensus $
    eval action
