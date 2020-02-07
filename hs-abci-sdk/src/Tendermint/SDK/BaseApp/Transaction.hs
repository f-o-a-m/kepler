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

import           Control.Lens                               ((&), (.~), (^.))
import           Control.Monad.IO.Class                     (liftIO)
import           Data.Proxy
import           Polysemy                                   (Embed, Members,
                                                             Sem)
import           Tendermint.SDK.BaseApp.Errors              (makeAppError,
                                                             txResultAppError)
import           Tendermint.SDK.BaseApp.Router              (Application,
                                                             RouteResult (..),
                                                             emptyDelayed,
                                                             runRouter)
import           Tendermint.SDK.BaseApp.Store               (ReadStore)
import           Tendermint.SDK.BaseApp.Transaction.Cache   (writeCache)
import           Tendermint.SDK.BaseApp.Transaction.Checker (DefaultCheckTx (..))
import           Tendermint.SDK.BaseApp.Transaction.Effect
import           Tendermint.SDK.BaseApp.Transaction.Router
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Types.TxResult              (TxResult,
                                                             txResultCode)
import           Data.ByteString                            (ByteString)
import           Data.Default.Class                         (def)
import           Data.IORef                                 (readIORef)
import           Tendermint.SDK.Types.Effects               ((:&))
import Control.Monad (when)
import Polysemy.Reader (Reader, runReader)

serveTxApplication
  :: HasTxRouter layout r c
  => HasWriteInContext c r
  => Members [Embed IO, ReadStore] r
  => Proxy layout
  -> Proxy r
  -> SRouteContext c
  -> RouteTx layout (TxEffs :& r) c
  -> TransactionApplication (Sem r)
serveTxApplication pl (pr :: Proxy r) (sc :: SRouteContext c) server tx = do
  let 
      router 
        :: TransactionContext
        -> Application (Sem (Reader TransactionContext ':r)) (RoutingTx ByteString) TxResult
      router ctx = runRouter (routeTx pl pr (Proxy @c) ctx (emptyDelayed (Route server))) ()

  ctx@TransactionContext{storeCache} <- liftIO $ newTransactionContext tx
  etxRes <- router ctx tx
  case etxRes of
    Fail e      -> pure $ def & txResultAppError .~ makeAppError e
    FailFatal e -> pure $ def & txResultAppError .~ makeAppError e
    Route txRes -> do
      (case sc of
        SCheckTx -> pure ()
        SDeliverTx ->
          when ((txRes ^. txResultCode) == 0) $ do
            c <- liftIO $ readIORef storeCache
            writeCache c
        ) :: Sem r ()
      pure txRes

serveDefaultTxChecker
  :: HasTxRouter layout r 'CheckTx
  => Members [Embed IO, ReadStore] r
  => DefaultCheckTx layout (TxEffs :& r)
  => RouteTx layout (TxEffs :& r) 'CheckTx ~ DefaultCheckTxT layout (TxEffs :& r)
  => Proxy layout
  -> Proxy r
  -> TransactionApplication (Sem r)
serveDefaultTxChecker pl (pr :: Proxy r) =
  serveTxApplication pl pr SCheckTx (defaultCheckTx pl (Proxy :: Proxy (TxEffs :& r)))
