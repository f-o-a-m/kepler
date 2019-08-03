module SimpleStorage.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , defaultHandler
  , transformHandler
  ) where

import           Control.Exception          (Exception, throwIO)
import           Control.Monad.Except       (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, runReaderT)
import           Data.Default.Class         (Default (..))
import qualified SimpleStorage.DB           as DB
import           SimpleStorage.StateMachine (initStateMachine)
import SimpleStorage.Transaction (TransactionError)

data AppConfig = AppConfig
  { countConnection :: DB.Connection "count"
  }

makeAppConfig :: IO AppConfig
makeAppConfig = do
  conn <- initStateMachine
  pure $ AppConfig { countConnection = conn
                   }

data AppError =
    QueryMissError String
  | TransactionError TransactionError
  deriving (Show)

instance Exception AppError

newtype Handler a = Handler
  { runHandler :: ReaderT AppConfig (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadError AppError, MonadIO)

-- NOTE: this should probably go in the library
defaultHandler
  :: ( Default a
     , Applicative m
     )
  => b
  -> m a
defaultHandler = const $ pure def

transformHandler
  :: AppConfig
  -> (forall a. Handler a -> IO a)
transformHandler cfg m = do
  eRes <- runExceptT $ runReaderT (runHandler m) cfg
  case eRes of
    Left e  -> throwIO e
    Right a -> pure a
