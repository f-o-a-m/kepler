{-# LANGUAGE UndecidableInstances #-}

module SimpleStorage.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler(..)
  , defaultHandler
  , transformHandler
  , runHandler
  ) where

import           Control.Exception                    (Exception)
import           Control.Lens                         ((&), (.~))
import           Control.Monad.Catch                  (throwM)
import           Data.Default.Class                   (Default (..))
import           Data.Text                            (Text, pack)
import           Network.ABCI.Server.App              (MessageType,
                                                       Response (..))
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader
import           SimpleStorage.Modules.SimpleStorage  as SimpleStorage
import           Tendermint.SDK.AuthTreeStore
import           Tendermint.SDK.Logger                as Logger
import           Tendermint.SDK.Store

data AppConfig = AppConfig
  { logConfig      :: Logger.LogConfig
  , authTreeDriver :: AuthTreeDriver
  }

makeAppConfig :: Logger.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  authTreeD <- initAuthTreeDriver
  pure $ AppConfig { logConfig = logCfg
                   , authTreeDriver = authTreeD
                   }

--------------------------------------------------------------------------------

data AppError = AppError String deriving (Show)

instance Exception AppError

printAppError :: AppError -> Text
printAppError (AppError msg) = pack $ "AppError : " <> msg

type EffR =
  [ SimpleStorage.SimpleStorage
  , Output SimpleStorage.Event
  , RawStore
  , Logger
  , Error AppError
  , Reader LogConfig
  , Embed IO
  ]

newtype Handler a = Handler { _runHandler :: Sem EffR a }
  deriving (Functor, Applicative, Monad)

-- NOTE: this should probably go in the library
defaultHandler
  :: ( Default a
     , Applicative m
     )
  => b
  -> m a
defaultHandler = const $ pure def

runHandler'
  :: AppConfig
  -> Handler a
  -> IO (Either AppError a)
runHandler' AppConfig{logConfig, authTreeDriver} (Handler m) = do
  runM .
    runReader logConfig .
    runError .
    Logger.evalKatip .
    interpretAuthTreeStore authTreeDriver .
    ignoreOutput @SimpleStorage.Event .
    SimpleStorage.eval $ m

runHandler
  :: AppConfig
  -> Handler a
  -> IO a
runHandler cfg m = do
  eRes <- runHandler' cfg m
  case eRes of
    Left e  -> throwM e
    Right a -> pure a

transformHandler
  :: AppConfig
  -> (forall (t :: MessageType). Handler (Response t) -> IO (Response t))
transformHandler cfg m = do
  eRes <- runHandler' cfg m
  case eRes of
    Left e  -> pure $ ResponseException $
      def & Resp._exceptionError .~ printAppError e
    Right a -> pure a
