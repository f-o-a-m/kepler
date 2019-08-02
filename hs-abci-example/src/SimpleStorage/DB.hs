module SimpleStorage.DB
  ( Connection
  , makeConnection
  , put
  , get
  , withHashTree
  , Transaction
  , TransactionError(..)
  , withConnection
  , abortTransaction
  , stageTransaction
  , commitTransaction
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, putMVar,
                                          readMVar)
import           Control.Monad.Except    (ExceptT, MonadError, runExceptT,
                                          throwError)
import           Control.Monad.IO.Class  (MonadIO (..))
import           Control.Monad.Reader    (MonadReader, ReaderT, ask, runReaderT)
import qualified Crypto.Data.Auth.Tree   as AT
import           Data.Binary             (Binary, decode, encode)
import           Data.ByteString         (ByteString)
import           Data.ByteString.Lazy    (fromStrict, toStrict)
import           Data.Proxy              (Proxy)
import           GHC.TypeLits            (Symbol)

data DB (name :: Symbol) = DB
  { dbTree :: AT.Tree ByteString ByteString
  }

newtype Connection name = Connection (MVar (DB name))

-- | Create a handle to a new DB.
makeConnection
  :: Proxy name
  -> IO (Connection name)
makeConnection _ = fmap Connection . newMVar $
  DB { dbTree = AT.empty
     }

-- | Put an item in the DB.
put
  :: Binary a
  => Connection name
  -> ByteString
  -> a
  -> IO ()
put (Connection c) k a =
  modifyMVar_ c $ \DB{dbTree} ->
    pure $ DB { dbTree = AT.insert k (toStrict . encode $ a) dbTree
              }

-- | Get an item from the DB from the key.
get
  :: forall a name.
     Binary a
  => Connection name
  -> ByteString
  -> IO (Maybe a)
get conn k = withHashTree conn $
  fmap (decode . fromStrict) . AT.lookup k

-- | Query the HashTree in a read-only fashion.
withHashTree
  :: Connection name
  -> (AT.Tree ByteString ByteString -> a)
  -> IO a
withHashTree (Connection c) f = do
  DB{dbTree} <- readMVar c
  pure $ f dbTree

data TransactionError = TransactionError String deriving Show

newtype Transaction name a =
  Transaction { runTransaction :: ReaderT (Connection name) (ExceptT TransactionError IO) a }
    deriving (Functor, Applicative, Monad, MonadError TransactionError, MonadReader (Connection name), MonadIO)

-- | Abort the transaction by throwing an error
abortTransaction
  :: String
  -> Transaction name a
abortTransaction = throwError . TransactionError

-- | Attempt to run the transaction, producting
-- either an error or an action to finalize (commit) the transaction
stageTransaction
  :: Connection name
  -> Transaction name a
  -> IO (Either TransactionError (a, IO ()))
stageTransaction (Connection c) transaction = do
  db <- readMVar c
  dbCopy <- newMVar db
  eTxRes <- runExceptT $ runReaderT (runTransaction transaction) (Connection dbCopy)
  pure $ case eTxRes of
    Left e -> Left e
    Right a ->
      let commitAction = readMVar dbCopy >>=  putMVar c
      in Right (a, commitAction)

-- | Run an commit a transaction
commitTransaction
  :: Connection name
  -> Transaction name a
  -> IO (Either TransactionError a)
commitTransaction conn transaction = do
  eRes <- stageTransaction conn transaction
  case eRes of
    Left e                  -> pure $ Left e
    Right (a, commitAction) ->  Right a <$ commitAction

-- | Use a connection to perform a transaction step
withConnection
  :: (Connection name -> IO a)
  -> Transaction name a
withConnection f = do
  conn <- ask
  liftIO $ f conn
