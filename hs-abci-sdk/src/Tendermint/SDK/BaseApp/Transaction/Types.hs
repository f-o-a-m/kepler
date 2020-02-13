{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.BaseApp.Transaction.Types
  ( module Tendermint.SDK.BaseApp.Transaction.Types
  -- * Re-Exports
  , Tx(..)
  ) where

import           Control.Lens                             (lens)
import           Data.ByteString                          (ByteString)
import           Data.IORef                               (IORef, newIORef)
import           Debug.Trace                              as Trace
import qualified Tendermint.SDK.BaseApp.Events            as E
import qualified Tendermint.SDK.BaseApp.Gas               as G
import           Tendermint.SDK.BaseApp.Router            (HasPath (..))
import qualified Tendermint.SDK.BaseApp.Transaction.Cache as Cache
import           Tendermint.SDK.Types.Transaction         (Tx (..))
import           Tendermint.SDK.Types.TxResult            (TxResult)

--------------------------------------------------------------------------------
-- Router Types and Combinators
--------------------------------------------------------------------------------

data msg :~> a

data TypedMessage msg

data Return a

data EmptyTxServer = EmptyTxServer

--------------------------------------------------------------------------------
-- RouteContext and Singletons
--------------------------------------------------------------------------------

data RouteContext = CheckTx | DeliverTx deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Transaction Application types
--------------------------------------------------------------------------------

data RoutingTx msg where
  RoutingTx :: Tx alg msg -> RoutingTx msg

instance Functor RoutingTx where
  fmap f (RoutingTx tx) = RoutingTx $ fmap f tx

instance HasPath (RoutingTx msg) where
  path = lens (\(RoutingTx tx) -> txRoute tx)
    (\(RoutingTx tx) r -> RoutingTx tx {txRoute = r})

data TransactionContext = TransactionContext
  { gasRemaining :: IORef G.GasAmount
  , storeCache   :: IORef Cache.Cache
  , events       :: IORef [E.Event]
  }

newTransactionContext
  :: RoutingTx msg
  -> IO TransactionContext
newTransactionContext (RoutingTx Tx{txGas}) = do
  initialGas <- newIORef $ G.GasAmount txGas
  Trace.traceM "creating new cache"
  initialCache <- newIORef Cache.emptyCache
  es <- newIORef []
  pure TransactionContext
    { gasRemaining = initialGas
    , storeCache = initialCache
    , events = es
    }

type TransactionApplication m =
  RoutingTx ByteString -> m (TxResult, Maybe Cache.Cache)

