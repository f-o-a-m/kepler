module Tendermint.SDK.BaseApp.Transaction.Effect
  ( TxEffs
  , newTransactionContext
  , runTx
  ) where

import           Control.Lens                             ((&), (.~))
import           Control.Monad.IO.Class                   (liftIO)
import qualified Data.ByteArray.Base64String              as Base64
import           Data.Default.Class                       (def)
import           Data.IORef                               (IORef, newIORef,
                                                           readIORef,
                                                           writeIORef)
import           Polysemy                                 (Embed, Member,
                                                           Members, Sem,
                                                           interpret,
                                                           raiseUnder, rewrite)
import           Polysemy.Error                           (Error, runError)
import           Polysemy.Internal                        (send)
import           Polysemy.Output                          (Output,
                                                           runOutputMonoidAssocR)
import qualified Polysemy.State                           as State
import           Polysemy.Tagged                          (Tagged (..))
import           Tendermint.SDK.BaseApp.Errors            (AppError,
                                                           txResultAppError)
import qualified Tendermint.SDK.BaseApp.Events            as E
import qualified Tendermint.SDK.BaseApp.Gas               as G
import           Tendermint.SDK.BaseApp.Store.RawStore    (ReadStore (..),
                                                           WriteStore (..))
import qualified Tendermint.SDK.BaseApp.Transaction.Cache as Cache
import           Tendermint.SDK.BaseApp.Transaction.Types (RoutingTx (..), Sing (SCheckTx, SDeliverTx),
                                                           StoreDeps)
import           Tendermint.SDK.Codec                     (HasCodec (encode))
import           Tendermint.SDK.Types.Effects             ((:&))
import           Tendermint.SDK.Types.Transaction         (Tx (..))
import           Tendermint.SDK.Types.TxResult            (TxResult,
                                                           txResultData,
                                                           txResultEvents,
                                                           txResultGasUsed,
                                                           txResultGasWanted)


type TxEffs =
    [ Output E.Event
    , G.GasMeter
    , WriteStore
    , ReadStore
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

runTx
  :: forall c r a.
     HasCodec a
  => Members [ReadStore, Embed IO] r
  => StoreDeps c r
  => Sing c
  -> TransactionContext
  -> Sem (TxEffs :& r) a
  -> Sem r TxResult
runTx routeContext ctx@TransactionContext {..} tx = do
  initialGas <- liftIO $ readIORef gas
  eRes <- eval ctx tx
  gasRemaining <- liftIO $ readIORef gas
  let gasUsed = initialGas - gasRemaining
      baseResponse =
        def & txResultGasWanted .~ G.unGasAmount initialGas
            & txResultGasUsed .~ G.unGasAmount gasUsed
  case eRes of
    Left e ->
      return $ baseResponse & txResultAppError .~ e
    Right (events, a) -> do
      (case routeContext of
        SCheckTx -> pure ()
        SDeliverTx -> do
          cache <- liftIO $ readIORef storeCache
          Cache.writeCache cache) :: Sem r ()
      return $ baseResponse & txResultEvents .~ events
                            & txResultData .~ Base64.fromBytes (encode a)


eval
  :: forall r a.
     Members [Embed IO, ReadStore] r
  => TransactionContext
  -> Sem (TxEffs :& r) a
  -> Sem r (Either AppError ([E.Event], a))
eval TransactionContext{gas, storeCache} =
  runError .
    evalCachedReadStore storeCache .
    rewrite (Tagged @Cache.Cache) .
    evalCachedWriteStore storeCache .
    rewrite (Tagged @Cache.Cache ) .
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
    (\(Tagged action) -> liftIO $ case action of
      StorePut k v  -> writeIORef c $ Cache.put k v cache
      StoreDelete k -> writeIORef c $ Cache.delete k cache
    ) m
