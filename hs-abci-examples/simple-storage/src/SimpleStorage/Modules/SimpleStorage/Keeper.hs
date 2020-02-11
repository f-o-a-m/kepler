{-# LANGUAGE TemplateHaskell #-}
module SimpleStorage.Modules.SimpleStorage.Keeper
  ( SimpleStorage
  , SimpleStorageEffs
  , storeKey
  , updateCount
  , getCount
  , eval
  ) where

import           Data.Maybe                                (fromJust)
import           Polysemy                                  (Members, Sem,
                                                            interpret, makeSem)
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

updateCount
  ::  Members '[SimpleStorage, Output BaseApp.Event] r
  => Count
  -> Sem r ()
updateCount count = do
  putCount count
  BaseApp.emit $ CountSet count

eval
  :: forall r.
     Members BaseApp.TxEffs r
  => forall a. (Sem (SimpleStorage ': r) a -> Sem r a)
eval = interpret (\case
  PutCount count -> BaseApp.put storeKey CountKey count
  GetCount -> fromJust <$> BaseApp.get storeKey CountKey
  )
