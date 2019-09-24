module SimpleStorage.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , defaultHandler
  , transformHandler
  , runHandler
  ) where

import           Control.Exception                    (Exception)
import           Control.Lens                         (lens, (&), (.~))
import           Control.Monad.Catch                  (throwM)
import           Control.Monad.Except                 (ExceptT, MonadError,
                                                       runExceptT)
import           Control.Monad.IO.Class               (MonadIO)
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       runReaderT)
import           Data.Default.Class                   (Default (..))
import           Data.Text                            (Text, pack)
import           Network.ABCI.Server.App              (MessageType,
                                                       Response (..))
import qualified Network.ABCI.Types.Messages.Response as Resp
import qualified SimpleStorage.Logging                as Log
import           SimpleStorage.StateMachine           (initStateMachine)
import qualified Tendermint.SDK.DB                    as DB

data AppConfig = AppConfig
  { countConnection :: DB.Connection "count"
  , logConfig       :: Log.LogConfig
  }

makeAppConfig :: Log.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  conn <- initStateMachine
  pure $ AppConfig { countConnection = conn
                   , logConfig = logCfg
                   }

data AppError = AppError String deriving (Show)

instance Exception AppError

printAppError :: AppError -> Text
printAppError (AppError msg) = pack $ "AppError : " <> msg

newtype Handler a = Handler
  { _runHandler :: ReaderT AppConfig (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadError AppError, MonadIO)

instance Log.HasLogConfig AppConfig where
  logConfig = lens g s
    where
      g = logConfig
      s cfg lc = cfg {logConfig = lc}

-- NOTE: this should probably go in the library
defaultHandler
  :: ( Default a
     , Applicative m
     )
  => b
  -> m a
defaultHandler = const $ pure def

runHandler
  :: AppConfig
  -> forall a. Handler a -> IO a
runHandler cfg m = do
  eRes <- runExceptT $ runReaderT (_runHandler m) cfg
  case eRes of
    Left e  -> throwM e
    Right a -> pure a


transformHandler
  :: AppConfig
  -> (forall (t :: MessageType). Handler (Response t) -> IO (Response t))
transformHandler cfg m = do
  eRes <- runExceptT $ runReaderT (_runHandler m) cfg
  case eRes of
    Left e  -> pure $ ResponseException $
      def & Resp._exceptionError .~ printAppError e
    Right a -> pure a
