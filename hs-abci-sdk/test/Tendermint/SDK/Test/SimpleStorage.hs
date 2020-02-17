{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.SimpleStorage
  ( SimpleStorageM
  , SimpleStorage
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
import           Data.Proxy
import qualified Data.Serialize                   as Serialize
import           Data.Serialize.Text              ()
import           Data.String.Conversions          (cs)
import           Data.Validation                  (Validation (..))
import           GHC.Generics                     (Generic)
import           Polysemy
import           Polysemy.Error                   (Error, throw)
import           Servant.API
import           Tendermint.SDK.Application       (Module (..), ModuleMembers)
import qualified Tendermint.SDK.BaseApp           as BA
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Types.Message     (HasMessageType (..),
                                                   Msg (..),
                                                   ValidateMessage (..))
import           Tendermint.SDK.Types.Transaction (Tx (..))


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Count = Count Int32 deriving (Eq, Show, Ord, Num, Serialize.Serialize)

data CountKey = CountKey

instance HasCodec Count where
    encode = Serialize.encode
    decode = first cs . Serialize.decode

instance BA.RawKey CountKey where
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance BA.IsKey CountKey "simple_storage" where
    type Value CountKey "simple_storage" = Count

instance BA.FromQueryData CountKey

instance BA.Queryable Count where
  type Name Count = "count"

--------------------------------------------------------------------------------
-- Message Types
--------------------------------------------------------------------------------

data UpdateCountTx = UpdateCountTx
  { updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

instance Serialize.Serialize UpdateCountTx

instance HasMessageType UpdateCountTx where
  messageType _ = "update_count"

instance HasCodec UpdateCountTx where
  encode = Serialize.encode
  decode = first cs . Serialize.decode

instance ValidateMessage UpdateCountTx where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------
-- Keeper
--------------------------------------------------------------------------------

storeKey :: BA.StoreKey "simple_storage"
storeKey = BA.StoreKey "simple_storage"

data SimpleStorage m a where
    PutCount :: Count -> SimpleStorage m ()
    GetCount :: SimpleStorage m (Maybe Count)

makeSem ''SimpleStorage

type SimpleStorageEffs = '[SimpleStorage]

updateCount
  ::  Member SimpleStorage r
  => Count
  -> Sem r ()
updateCount count = putCount count

eval
  :: forall r.
     Members BA.TxEffs r
  => forall a. (Sem (SimpleStorage ': r) a -> Sem r a)
eval = interpret (\case
  PutCount count -> BA.put storeKey CountKey count
  GetCount -> BA.get storeKey CountKey
  )

--------------------------------------------------------------------------------
-- Router
--------------------------------------------------------------------------------

type MessageApi =
  BA.TypedMessage UpdateCountTx BA.:~> BA.Return ()

messageHandlers
  :: Member SimpleStorage r
  => Members BA.TxEffs r
  => BA.RouteTx MessageApi r
messageHandlers = updateCountH

updateCountH
  :: Member SimpleStorage r
  => Members BA.TxEffs r
  => BA.RoutingTx UpdateCountTx
  -> Sem r ()
updateCountH (BA.RoutingTx Tx{txMsg}) =
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
  :> BA.Leaf Count

getMultipliedCount
  :: Members [Error BA.AppError, SimpleStorage] r
  => Integer
  -> Integer
  -> Sem r (BA.QueryResult Count)
getMultipliedCount subtractor multiplier = do
  let m = fromInteger multiplier
      s = fromInteger subtractor
  mc <- getCount
  case mc of
    Nothing -> throw . BA.makeAppError $ BA.ResourceNotFound
    Just c -> pure $ BA.QueryResult
      { queryResultData = m * c - s
      , queryResultIndex = 0
      , queryResultKey = Base64.fromBytes $ CountKey ^. BA.rawKey
      , queryResultProof  = Nothing
      , queryResultHeight = 0
      }

type QueryApi = GetMultipliedCount :<|> BA.QueryApi CountStoreContents

querier
  :: forall r.
     Members BA.QueryEffs r
  => Member SimpleStorage r
  => BA.RouteQ QueryApi r
querier =
  let storeHandlers = BA.storeQueryHandlers (Proxy :: Proxy CountStoreContents)
        storeKey (Proxy :: Proxy r)
  in getMultipliedCount :<|> storeHandlers

--------------------------------------------------------------------------------
-- Module Definition
--------------------------------------------------------------------------------

type SimpleStorageM =
  Module "simple_storage" MessageApi MessageApi QueryApi SimpleStorageEffs '[]

simpleStorageModule
  :: ModuleMembers SimpleStorageM r
  => SimpleStorageM r
simpleStorageModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = BA.defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }

evalToIO
  :: BA.PureContext
  -> Sem (BA.BaseApp BA.PureCoreEffs) a
  -> IO a
evalToIO context action = do
  eRes <- BA.runPureCoreEffs context .  BA.defaultCompileToPureCore $ action
  either (error . show) pure eRes
