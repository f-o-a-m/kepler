module SimpleStorage.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , defaultHandler
  , transformHandler
  ) where

import           Control.Lens                         ((&), (.~), lens)
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
import           SimpleStorage.StateMachine           (initStateMachine)
import qualified Tendermint.SDK.DB                    as DB
import qualified SimpleStorage.Logging as Log

data AppConfig = AppConfig
  { countConnection :: DB.Connection "count"
  , loggingConfig :: Log.LogConfig
  }

makeAppConfig :: IO AppConfig
makeAppConfig = do
  conn <- initStateMachine
  logCfg <- Log.mkLogConfig "simple-storage"
  pure $ AppConfig { countConnection = conn
                   , loggingConfig = logCfg
                   }

data AppError = AppError String deriving (Show)

printAppError :: AppError -> Text
printAppError (AppError msg) = pack $ "AppError : " <> msg

newtype Handler a = Handler
  { runHandler :: ReaderT AppConfig (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader AppConfig, MonadError AppError, MonadIO)

instance Log.HasLogConfig AppConfig where
  logConfig = lens g s
    where
      g = loggingConfig
      s cfg lc = cfg {loggingConfig = lc} 

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
  -> (forall (t :: MessageType). Handler (Response t) -> IO (Response t))
transformHandler cfg m = do
  eRes <- runExceptT $ runReaderT (runHandler m) cfg
  case eRes of
    Left e  -> pure $ ResponseException $
      def & Resp._exceptionError .~ printAppError e
    Right a -> pure a
