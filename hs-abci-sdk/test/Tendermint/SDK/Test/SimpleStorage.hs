{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.SimpleStorage where

import           Control.Lens                     (iso)
import           Crypto.Hash                      (SHA256 (..), hashWith)
import           Data.Bifunctor                   (first)
import           Data.ByteArray                   (convert)
import           Data.ByteString                  (ByteString)
import           Data.Int                         (Int32)
import           Data.Maybe                       (fromJust)
import           Data.Proxy
import qualified Data.Serialize                   as Serialize
import           Data.Serialize.Text              ()
import qualified Data.Serialize.Text              ()
import           Data.String.Conversions          (cs)
import           GHC.Generics                     (Generic)
import           Polysemy
import           Polysemy.Error                   (Error)
import           Tendermint.SDK.Application       (Module (..))
import qualified Tendermint.SDK.BaseApp           as BaseApp
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Types.Message     (Msg (..))
import           Tendermint.SDK.Types.Transaction (PreRoutedTx (..), Tx (..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype Count = Count Int32 deriving (Eq, Show, Serialize.Serialize)

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

data SimpleStorageMessage =
  UpdateCount UpdateCountTx

data UpdateCountTx = UpdateCountTx
  { updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

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

router
  :: Member SimpleStorage r
  => Members BaseApp.TxEffs r
  => PreRoutedTx SimpleStorageMessage
  -> Sem r ()
router (PreRoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       UpdateCount UpdateCountTx{updateCountTxCount} ->
          updateCount (Count updateCountTxCount)

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

type CountStoreContents = '[(CountKey, Count)]

type Api = BaseApp.QueryApi CountStoreContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteT Api (Sem r)
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy CountStoreContents)
    storeKey (Proxy :: Proxy (Sem r))

--------------------------------------------------------------------------------
-- Module Definition
--------------------------------------------------------------------------------

type SimpleStorageM r =
  Module "simple_storage" SimpleStorageMessage () Api SimpleStorageEffs r

simpleStorageModule
  :: Member SimpleStorage r
  => Members BaseApp.BaseAppEffs r
  => SimpleStorageM r
simpleStorageModule = Module
  { moduleTxDeliverer = router
  , moduleTxChecker = router
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
