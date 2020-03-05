{-# LANGUAGE TemplateHaskell #-}

module SimpleStorage.Modules.SimpleStorage.Keeper
  ( SimpleStorageEffs
  , SimpleStorageKeeper(..)
  , updateCount
  , getCount
  , eval
  --
  , countVar
  ) where

import           Control.Lens                              (iso)
import           Crypto.Hash                               (SHA256 (..),
                                                            hashWith)
import           Data.ByteArray                            (convert)
import           Data.ByteString                           (ByteString)
import           Data.String.Conversions                   (cs)
import           Polysemy                                  (Members, Sem,
                                                            interpret, makeSem)
import           Polysemy.Output                           (Output)
import           SimpleStorage.Modules.SimpleStorage.Types
import qualified Tendermint.SDK.BaseApp                    as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Var          as V

type SimpleStorageEffs = '[SimpleStorageKeeper]

data SimpleStorageKeeper m a where
    UpdateCount :: Count -> SimpleStorageKeeper m ()
    GetCount :: SimpleStorageKeeper m (Maybe Count)

makeSem ''SimpleStorageKeeper

eval
  :: forall r.
     Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => forall a. (Sem (SimpleStorageKeeper ': r) a -> Sem r a)
eval = interpret (\case
  UpdateCount count -> updateCountF count
  GetCount -> V.takeVar countVar
  )

updateCountF
  :: Members '[BaseApp.WriteStore, Output BaseApp.Event, BaseApp.Logger] r
  => Count
  -> Sem r ()
updateCountF count = do
  V.putVar count countVar
  let event = CountSet count
  BaseApp.emit event
  BaseApp.logEvent event


--------------------------------------------------------------------------------

data SimpleStorageNamespace

store :: BaseApp.Store SimpleStorageNamespace
store = BaseApp.makeStore $ BaseApp.KeyRoot "simple_storage"

data CountKey = CountKey

instance BaseApp.RawKey CountKey where
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance BaseApp.IsKey CountKey SimpleStorageNamespace where
    type Value CountKey SimpleStorageNamespace = V.Var Count

countVar :: V.Var Count
countVar = V.makeVar CountKey store

instance BaseApp.QueryData CountKey
