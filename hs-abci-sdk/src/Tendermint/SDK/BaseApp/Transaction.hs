module Tendermint.SDK.BaseApp.Transaction
  ( TxEffs
  , TransactionContext(..)
  , newTransactionContext
  , eval
  ) where

import           Control.Lens                     ((&), (.~))
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteArray.Base64String      as Base64
import           Data.Default.Class               (def)
import           Data.IORef                       (IORef, newIORef, readIORef, writeIORef)
import           Polysemy.Internal                         (send)
import           Polysemy                         (Embed, Member, Sem,
                                                   raiseUnder, interpret)
import           Polysemy.Error                   (Error, runError)
import           Polysemy.Output                  (Output,
                                                   runOutputMonoidAssocR)
import           Polysemy.State                   (State, runStateIORef)
import           Polysemy.Tagged                       (Tagged (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.ByteString (ByteString)
import Tendermint.SDK.BaseApp.Store.RawStore (RawStore(..), StoreKey(..))
import           Tendermint.SDK.BaseApp.Errors    (AppError, txResultAppError, throwSDKError, SDKError(..))
import qualified Tendermint.SDK.BaseApp.Events    as E
import qualified Tendermint.SDK.BaseApp.Gas       as G
import           Tendermint.SDK.Codec             (HasCodec (encode))
import           Tendermint.SDK.Types.Effects     ((:&))
import           Tendermint.SDK.Types.Transaction (RoutedTx (..), Tx (..))
import           Tendermint.SDK.Types.TxResult    (TxResult, txResultData,
                                                   txResultEvents,
                                                   txResultGasUsed,
                                                   txResultGasWanted)
data Cache
type TxEffs =
    [ Output E.Event
    , G.GasMeter
    , Tagged Cache RawStore
    , Error AppError
    ]

data TransactionContext = TransactionContext
  { gas :: IORef G.GasAmount
  , storeCache :: IORef (Map ByteString ByteString)
  }

newTransactionContext
  :: RoutedTx msg
  -> IO TransactionContext
newTransactionContext (RoutedTx Tx{txGas}) = do
  initialGas <- newIORef $ G.GasAmount txGas
  initialCache <- newIORef $ mempty
  pure TransactionContext
    { gas = initialGas
    , storeCache = initialCache
    }

eval
  :: forall r a.
     HasCodec a
  => Member (Embed IO) r
  => Member RawStore r
  => TransactionContext
  -> Sem (TxEffs :& r) a
  -> Sem r TxResult
eval TransactionContext{..} action = do
  initialGas <- liftIO $ readIORef gas
  eRes <-
    runError .
      evalCachedStore storeCache .
      runStateIORef gas .
      G.eval .
      raiseUnder @(State G.GasAmount) $ runOutputMonoidAssocR (pure @[])  action
  gasRemaining <- liftIO $ readIORef gas
  let gasUsed = initialGas - gasRemaining
      baseResponse =
        def & txResultGasWanted .~ G.unGasAmount initialGas
            & txResultGasUsed .~ G.unGasAmount gasUsed
  return $ case eRes of
    Left e ->
      baseResponse & txResultAppError .~ e
    Right (events, a) ->
      baseResponse & txResultEvents .~ events
                   & txResultData .~ Base64.fromBytes (encode a)

evalCachedStore
  :: Member RawStore r
  => Member (Embed IO) r
  => Member (Error AppError) r
  => IORef (Map ByteString ByteString)
  -> Sem (Tagged Cache RawStore ': r) a
  -> Sem r a
evalCachedStore cache m = do
  interpret
    (\(Tagged action) -> case action of
      RawStorePut (StoreKey sk) k v -> liftIO $ do
        cm <-  readIORef cache
        cache `writeIORef` Map.insert (sk <> k) v cm
      RawStoreGet sk'@(StoreKey sk) k -> do
        mv <- Map.lookup (sk <> k) <$> liftIO (readIORef cache)
        case mv of
          Just v -> pure (Just v)
          Nothing -> send (RawStoreGet sk' k)
      RawStoreProve _ _ -> pure Nothing
      RawStoreDelete (StoreKey sk) k -> liftIO $ do
        cm <-  readIORef cache
        cache `writeIORef` Map.delete (sk <> k) cm
      RawStoreRoot -> throwSDKError $ CacheStoreUnsupportedOperation "Root"
      RawStoreBeginTransaction -> throwSDKError $ CacheStoreUnsupportedOperation "BeginTransaction"
      RawStoreRollback -> liftIO $ cache `writeIORef`  mempty
      RawStoreCommit -> throwSDKError $ CacheStoreUnsupportedOperation "Commit"
    ) m
