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
import           Data.Default.Class                   (Default (..))
import           Data.Text                            (Text, pack)
import           Network.ABCI.Server.App              (MessageType,
                                                       Response (..))
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Polysemy
import           Polysemy.Error
import           Polysemy.Input
import qualified SimpleStorage.Logging                as Log
import           Tendermint.SDK.Module
import           Tendermint.SDK.Subscription

data AppConfig = AppConfig
  { logConfig       :: Log.LogConfig
  }

makeAppConfig :: Log.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  pure $ AppConfig { logConfig = logCfg
                   }

data AppError = AppError String deriving (Show)

instance Exception AppError

printAppError :: AppError -> Text
printAppError (AppError msg) = pack $ "AppError : " <> msg

newtype Handler a = Handler
  { _runHandler :: Handler (Sem (Input ': BaseAppR) a) }

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
