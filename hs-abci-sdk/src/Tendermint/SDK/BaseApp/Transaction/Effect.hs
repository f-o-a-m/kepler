module Tendermint.SDK.BaseApp.Transaction.Effect
  ( TxEffs
  , newTransactionContext
  , eval
  ) where

import           Control.Monad.IO.Class                   (liftIO)
import           Data.IORef                               (IORef, newIORef,
                                                           readIORef,
                                                           writeIORef)
import           Polysemy                                 (Embed, Member,
                                                           Members, Sem,
                                                           interpret,
                                                           raiseUnder)
import           Polysemy.Error                           (Error, runError)
import           Polysemy.Internal                        (send)
import           Polysemy.Output                          (Output,
                                                           runOutputMonoidAssocR)
import qualified Polysemy.State                           as State
import           Polysemy.Tagged                          (Tagged (..))
import           Tendermint.SDK.BaseApp.Errors            (AppError)
import qualified Tendermint.SDK.BaseApp.Events            as E
import qualified Tendermint.SDK.BaseApp.Gas               as G
import           Tendermint.SDK.BaseApp.Store.RawStore    (ReadStore (..),
                                                           WriteStore (..))
import qualified Tendermint.SDK.BaseApp.Transaction.Cache as Cache
import           Tendermint.SDK.BaseApp.Transaction.Types (RoutingTx (..))
import           Tendermint.SDK.Types.Effects             ((:&))
import           Tendermint.SDK.Types.Transaction         (Tx (..))


type TxEffs =
    [ Output E.Event
    , G.GasMeter
    , Tagged Cache.Cache WriteStore
    , Tagged Cache.Cache ReadStore
    , Error AppError
    ]

data TransactionContext = TransactionContext
  { gas        :: IORef G.GasAmount
  , storeCache :: IORef Cache.Cache
  }

newTransactionContext
  :: RoutingTx msg
  -> IO TransactionContext
newTransactionContext (RoutingTx Tx{txGas}) = do
  initialGas <- newIORef $ G.GasAmount txGas
  initialCache <- newIORef $ Cache.emptyCache
  pure TransactionContext
    { gas = initialGas
    , storeCache = initialCache
    }

eval
  :: forall r a.
     Members [Embed IO, ReadStore] r
  => TransactionContext
  -> Sem (TxEffs :& r) a
  -> Sem r (Either AppError ([E.Event], a))
eval TransactionContext{gas, storeCache} = do
  runError .
    evalCachedReadStore storeCache .
    evalCachedWriteStore storeCache .
    State.runStateIORef gas .
    G.eval .
    raiseUnder @(State.State G.GasAmount) .
    runOutputMonoidAssocR (pure @[])

evalCachedReadStore
  :: Members [Embed IO, ReadStore] r
  => IORef Cache.Cache
  -> Sem (Tagged Cache.Cache ReadStore ': r) a
  -> Sem r a
evalCachedReadStore c m = do
  cache <- liftIO $ readIORef c
  interpret
    (\(Tagged action) -> case action of
      StoreGet k ->
        case Cache.get k cache of
          Left Cache.Deleted -> pure Nothing
          Right (Just v)     -> pure (Just v)
          Right Nothing      -> send (StoreGet k)
      StoreProve _ -> pure Nothing
      StoreRoot _ -> pure ""
    ) m

evalCachedWriteStore
  :: Member (Embed IO) r
  => IORef Cache.Cache
  -> Sem (Tagged Cache.Cache WriteStore ': r) a
  -> Sem r a
evalCachedWriteStore c m = do
  cache <- liftIO $ readIORef c
  interpret
    (liftIO . \(Tagged action) -> case action of
      StorePut k v  -> writeIORef c $ Cache.put k v cache
      StoreDelete k -> writeIORef c $ Cache.delete k cache
    ) m
