{-# LANGUAGE QuasiQuotes     #-}
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

import           Polysemy                                  (Members, Sem,
                                                            interpret, makeSem)
import           Polysemy.Output                           (Output)
import           SimpleStorage.Modules.SimpleStorage.Keys  (countKey)
import           SimpleStorage.Modules.SimpleStorage.Types
import qualified Tendermint.SDK.BaseApp                    as BaseApp
import           Tendermint.SDK.BaseApp.Store.TH           (makeSubStore)
import qualified Tendermint.SDK.BaseApp.Store.Var          as V


--------------------------------------------------------------------------------

data SimpleStorageNamespace

store :: BaseApp.Store SimpleStorageNamespace
store = BaseApp.makeStore $ BaseApp.KeyRoot "simple_storage"

$(makeSubStore 'store "countVar" [t| V.Var Count |] countKey)

instance BaseApp.QueryData CountVarKey

--------------------------------------------------------------------------------

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
