module Tendermint.SDK.BaseApp.Transaction
  ( serveTxApplication
  , serveDefaultTxChecker
    -- * Re-Exports
  , module Tendermint.SDK.BaseApp.Transaction.Types
  , HasTxRouter(..)
  , emptyTxServer
  , DefaultCheckTx(..)
  , TxEffs
  ) where

import           Control.Lens                               ((&), (.~))
import           Data.Proxy
import           Polysemy                                   (Sem)
import           Tendermint.SDK.BaseApp.Errors              (makeAppError,
                                                             txResultAppError)
import           Tendermint.SDK.BaseApp.Router              (Application,
                                                             RouteResult (..),
                                                             emptyDelayed,
                                                             runRouter)
import           Tendermint.SDK.BaseApp.Transaction.Checker (DefaultCheckTx (..))
import           Tendermint.SDK.BaseApp.Transaction.Effect  (TxEffs)
import           Tendermint.SDK.BaseApp.Transaction.Router
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Types.TxResult              (TxResult)

import           Data.ByteString                            (ByteString)
import           Data.Default.Class                         (def)
import Data.Singletons (Sing, sing)

serveTxApplication
  :: HasTxRouter layout r c
  => Proxy layout
  -> Proxy r
  -> Sing (c :: RouteContext)
  -> RouteTx layout r c
  -> TransactionApplication (Sem r)
serveTxApplication pl pr sc server =
  toTxApplication (runRouter (routeTx pl pr sc (emptyDelayed (Route server))) ())

toTxApplication
  :: Application (Sem r) (RoutingTx ByteString) TxResult
  -> TransactionApplication (Sem r)
toTxApplication ra tx = do
  res <- ra tx
  case res of
    Fail e      -> pure $ def & txResultAppError .~ makeAppError e
    FailFatal e -> pure $ def & txResultAppError .~ makeAppError e
    Route a     -> pure a


serveDefaultTxChecker
  :: HasTxRouter layout r 'CheckTx
  => DefaultCheckTx layout r
  => RouteTx layout r 'CheckTx ~ DefaultCheckTxT layout r
  => Proxy layout
  -> Proxy r
  -> TransactionApplication (Sem r)
serveDefaultTxChecker pl pr =
  serveTxApplication pl pr (sing :: Sing 'CheckTx) (defaultCheckTx pl pr)
