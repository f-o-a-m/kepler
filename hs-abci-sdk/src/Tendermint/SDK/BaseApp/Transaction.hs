module Tendermint.SDK.BaseApp.Transaction
  ( serveTxApplication
  , serveDefaultTxChecker
    -- * Re-Exports
  , module Tendermint.SDK.BaseApp.Transaction.Types
  , HasTxRouter(..)
  , emptyTxServer
  , DefaultCheckTx(..)
  , TxEffs
  , evalReadOnly
  ) where

import           Control.Lens                               ((&), (.~))
import           Data.ByteString                            (ByteString)
import           Data.Default.Class                         (def)
import           Data.Proxy
import           Polysemy                                   (Sem)
import           Tendermint.SDK.BaseApp.Errors              (makeAppError,
                                                             txResultAppError)
import           Tendermint.SDK.BaseApp.Router              (Application,
                                                             RouteResult (..),
                                                             emptyDelayed,
                                                             runRouter)
import           Tendermint.SDK.BaseApp.Transaction.Cache   (Cache)
import           Tendermint.SDK.BaseApp.Transaction.Checker
import           Tendermint.SDK.BaseApp.Transaction.Effect
import           Tendermint.SDK.BaseApp.Transaction.Router
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Types.Effects               ((:&))
import           Tendermint.SDK.Types.TxResult              (TxResult)

serveTxApplication
  :: HasTxRouter layout r
  => Proxy layout
  -> Proxy r
  -> RouteTx layout (TxEffs :& r)
  -> TransactionApplication (Sem r)
serveTxApplication pl pr server =
  toTxApplication (runRouter (routeTx pl pr (emptyDelayed (Route server))) ())

toTxApplication
  :: Application (Sem r) (RoutingTx ByteString) (TxResult, Maybe Cache)
  -> TransactionApplication (Sem r)
toTxApplication ra tx = do
  res <- ra tx
  case res of
    Fail e      -> pure (def & txResultAppError .~ makeAppError e, Nothing)
    FailFatal e -> pure (def & txResultAppError .~ makeAppError e, Nothing)
    Route a     -> pure a

serveDefaultTxChecker
  :: HasTxRouter (VoidReturn layout) r
  => DefaultCheckTx layout (TxEffs :& r)
  => RouteTx (VoidReturn layout) (TxEffs :& r) ~ DefaultCheckTxT layout (TxEffs :& r)
  => Proxy layout
  -> Proxy r
  -> TransactionApplication (Sem r)
serveDefaultTxChecker (pl :: Proxy layout) (pr :: Proxy r) =
  serveTxApplication (Proxy :: Proxy (VoidReturn layout)) pr (defaultCheckTx pl (Proxy :: Proxy (TxEffs :& r)))
