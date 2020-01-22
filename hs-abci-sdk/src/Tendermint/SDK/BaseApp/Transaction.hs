module Tendermint.SDK.BaseApp.Transaction
  ( TransactionApplication
  , serveTxApplication
  , serveDefaultTxChecker
    -- * Re-Exports
  , PreRoutedTx(..)
  , HasTxRouter(..)
  , (:~>)
  , TypedMessage
  , TxEffs
  ) where

import           Data.Proxy
import           Polysemy                                   (Sem)
import           Tendermint.SDK.BaseApp.Router              (RouteResult (..),
                                                             emptyDelayed,
                                                             runRouter)
import           Tendermint.SDK.BaseApp.Transaction.Checker (DefaultCheckTx (..))
import           Tendermint.SDK.BaseApp.Transaction.Effect  (TxEffs)
import           Tendermint.SDK.BaseApp.Transaction.Router  (HasTxRouter (..))
import           Tendermint.SDK.BaseApp.Transaction.Types


serveTxApplication
  :: HasTxRouter layout r c
  => Proxy layout
  -> Proxy r
  -> Proxy (c :: RouteContext)
  -> RouteTx layout r c
  -> TransactionApplication (Sem r)
serveTxApplication pl pr pc server =
  runRouter (routeTx pl pr pc (emptyDelayed (Route server))) ()

serveDefaultTxChecker
  :: HasTxRouter layout r 'CheckTx
  => DefaultCheckTx layout r
  => RouteTx layout r 'CheckTx ~ DefaultCheckTxT layout r
  => Proxy layout
  -> Proxy r
  -> TransactionApplication (Sem r)
serveDefaultTxChecker pl pr =
  serveTxApplication pl pr (Proxy :: Proxy 'CheckTx) (defaultCheckTxServer pl pr)
