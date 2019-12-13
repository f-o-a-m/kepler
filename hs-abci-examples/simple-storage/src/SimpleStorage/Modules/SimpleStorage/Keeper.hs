{-# LANGUAGE TemplateHaskell #-}
module SimpleStorage.Modules.SimpleStorage.Keeper
  ( SimpleStorage
  , SimpleStorageEffs
  , storeKey
  , putCount
  , getCount
  , eval
  ) where

import           Data.Maybe                                (fromJust)
import           Polysemy                                  (Members, Sem,
                                                            interpret, makeSem)
import           Polysemy.Error                            (Error)
import           Polysemy.Output                           (Output)
import           SimpleStorage.Modules.SimpleStorage.Types (Count,
                                                            CountKey (..),
                                                            CountSet (..))
import qualified Tendermint.SDK.BaseApp                    as BaseApp

storeKey :: BaseApp.StoreKey "simple_storage"
storeKey = BaseApp.StoreKey "simple_storage"

data SimpleStorage m a where
    PutCount :: Count -> SimpleStorage m ()
    GetCount :: SimpleStorage m Count

makeSem ''SimpleStorage

type SimpleStorageEffs = '[SimpleStorage]

eval
  :: forall r.
     Members '[BaseApp.RawStore, Output BaseApp.Event, Error BaseApp.AppError] r
  => forall a. (Sem (SimpleStorage ': r) a -> Sem r a)
eval = interpret (\case
  PutCount count -> do
    BaseApp.put storeKey CountKey count
    BaseApp.emit $ CountSet count

  GetCount -> fromJust <$> BaseApp.get storeKey CountKey
  )
