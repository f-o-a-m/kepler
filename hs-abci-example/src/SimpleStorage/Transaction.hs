module SimpleStorage.Transaction
  ( Transaction
  , TransactionState(..)
  , TransactionError(..)
  , abortTransaction
  , stageTransaction
  , commitTransaction
  , stateChange
  ) where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TVar (writeTVar, readTVar)
import           Control.Monad.Except  (Except, MonadError, runExcept,
                                        throwError)
import           Control.Monad.State
import           Data.Monoid (Endo(..))
import           SimpleStorage.DB      (Connection(..),DB)



data TransactionError = TransactionError String deriving Show

data TransactionState name = TransactionState
  { originalRoot :: DB name
  , currentRoot :: DB name
  }

newtype Transaction name a =
  Transaction { runTransaction :: StateT (TransactionState name) (Except TransactionError) a }
    deriving (Functor, Applicative, Monad, MonadError TransactionError, MonadState (TransactionState name))

-- | Abort the transaction by throwing an error
abortTransaction
  :: String
  -> Transaction name a
abortTransaction = throwError . TransactionError

stageTransaction
  :: DB name
  -> Transaction name a
  -> Either TransactionError (a, TransactionState name)
stageTransaction db transaction =
  let initialTxState =
        TransactionState { originalRoot = db
                         , currentRoot = db
                         }
  in runExcept $ runStateT (runTransaction transaction) initialTxState

commitTransaction
  :: Connection name
  -> Transaction name a
  -> IO (Either TransactionError a)
commitTransaction (Connection c) transaction = atomically $ do
  db <- readTVar c
  case stageTransaction db transaction of
    Left e -> pure $ Left e
    Right (a, TransactionState {currentRoot}) -> do
      writeTVar c currentRoot
      pure $ Right a

stateChange
  :: Endo (DB name)
  -> Transaction name ()
stateChange f = modify $ \txState ->
  txState {currentRoot = appEndo f $ currentRoot txState}
