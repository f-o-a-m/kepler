{-# LANGUAGE TemplateHaskell #-}
module SimpleStorage.Modules.SimpleStorage.Keeper
  ( SimpleStorageKeeper
  , SimpleStorageEffs
  , updateCount
  , getCount
  , storeKey
  , eval
  ) where

import           Polysemy                                  (Members, Sem,
                                                            interpret, makeSem)
import           Polysemy.Output                           (Output)
import           SimpleStorage.Modules.SimpleStorage.Types
import qualified Tendermint.SDK.BaseApp                    as BaseApp

storeKey :: BaseApp.StoreKey SimpleStorageName
storeKey = BaseApp.StoreKey "simple_storage"

data SimpleStorageKeeper m a where
    PutCount :: Count -> SimpleStorageKeeper m ()
    GetCount :: SimpleStorageKeeper m (Maybe Count)

makeSem ''SimpleStorageKeeper

type SimpleStorageEffs = '[SimpleStorageKeeper]

updateCount
  ::  Members '[SimpleStorageKeeper, Output BaseApp.Event, BaseApp.Logger] r
  => Count
  -> Sem r ()
updateCount count = do
  putCount count
  let event = CountSet count
  BaseApp.emit event
  BaseApp.logEvent event

eval
  :: forall r.
     Members BaseApp.TxEffs r
  => forall a. (Sem (SimpleStorageKeeper ': r) a -> Sem r a)
eval = interpret (\case
  PutCount count -> do
    BaseApp.put storeKey CountKey count
  GetCount -> BaseApp.get storeKey CountKey
  )
