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
                                                             txResultData,
                                                             txResultEvents,
                                                             txResultGasUsed,
                                                             txResultGasWanted)

import           Data.ByteArray.Base64String                (fromBytes)
import           Data.ByteString                            (ByteString)
import           Data.Default.Class                         (def)
import           Data.IORef                                 (readIORef)
import qualified Tendermint.SDK.BaseApp.Gas                 as G
import           Tendermint.SDK.Types.Effects               ((:&))

serveTxApplication
  :: HasTxRouter layout (TxEffs :& r) c
  => HasWriteInContext c r
  => Members [Embed IO, ReadStore] r
  => Proxy layout
  -> Proxy (TxEffs :& r)
  -> SRouteContext c
  -> RouteTx layout (TxEffs :& r) c
  -> TransactionApplication (Sem r)
serveTxApplication pl pr (sc :: SRouteContext c) server =
  toTxApplication sc (runRouter (routeTx pl pr (Proxy @c) (emptyDelayed (Route server))) ())

toTxApplication
  :: Members [Embed IO, ReadStore] r
  => HasWriteInContext c r
  => SRouteContext c
  -> Application (Sem (TxEffs :& r)) (RoutingTx ByteString) ByteString
  -> TransactionApplication (Sem r)
toTxApplication sc ra tx = do
  ctx <- liftIO $ newTransactionContext tx
  runTx sc ctx $ ra tx

runTx
  :: Members [Embed IO, ReadStore] r
  => HasWriteInContext c r
  => SRouteContext c
  -> TransactionContext
  -> Sem (TxEffs :& r) (RouteResult ByteString)
  -> Sem r TxResult
runTx sc ctx@TransactionContext{..} (tx :: Sem (TxEffs :& r) (RouteResult ByteString)) = do
  initialGas <- liftIO $ readIORef gasRemaining
  eRes <- eval ctx tx
  finalGas <- liftIO $ readIORef gasRemaining
  let gasUsed = initialGas - finalGas
      baseResponse =
        def & txResultGasWanted .~ G.unGasAmount initialGas
            & txResultGasUsed .~ G.unGasAmount gasUsed
  case eRes of
    Left e -> return $ baseResponse & txResultAppError .~ e
    Right res -> case res of
      Fail e      -> return $ baseResponse & txResultAppError .~ makeAppError e
      FailFatal e -> return $ baseResponse & txResultAppError .~ makeAppError e
      Route a -> do
        (case sc of
          SCheckTx -> pure ()
          SDeliverTx -> do
            c <- liftIO $ readIORef storeCache
            writeCache c) :: Sem r ()
        es <- liftIO $ readIORef events
        return $ baseResponse & txResultEvents .~ es
                              & txResultData .~ fromBytes a

serveDefaultTxChecker
  :: HasTxRouter layout (TxEffs :& r) 'CheckTx
  => Members [Embed IO, ReadStore] r
  => DefaultCheckTx layout (TxEffs :& r)
  => RouteTx layout (TxEffs :& r) 'CheckTx ~ DefaultCheckTxT layout (TxEffs :& r)
  => Proxy layout
  -> Proxy (TxEffs :& r)
  -> TransactionApplication (Sem r)
serveDefaultTxChecker pl pr =
  serveTxApplication pl pr SCheckTx (defaultCheckTx pl pr)
