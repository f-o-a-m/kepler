{-# LANGUAGE UndecidableInstances #-}

module SimpleStorage.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , runHandler
  ) where

import           Control.Exception                   (Exception)
import           Control.Monad.Catch                 (throwM)
import           Polysemy
import           Polysemy.Error
import           Polysemy.Output
import           Polysemy.Reader
import           SimpleStorage.Modules.SimpleStorage as SimpleStorage
import           Tendermint.SDK.AuthTreeStore
import qualified Tendermint.SDK.Events               as Events
import           Tendermint.SDK.Logger               as Logger
import           Tendermint.SDK.Store

data AppConfig = AppConfig
  { logConfig      :: Logger.LogConfig
  , authTreeDriver :: AuthTreeDriver
  , eventBuffer    :: Events.EventBuffer
  }

makeAppConfig :: Logger.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  authTreeD <- initAuthTreeDriver
  eb <- Events.newEventBuffer
  pure $ AppConfig { logConfig = logCfg
                   , authTreeDriver = authTreeD
                   , eventBuffer = eb
                   }

--------------------------------------------------------------------------------

data AppError = AppError String deriving (Show)

instance Exception AppError

type EffR =
  [ SimpleStorage.SimpleStorage
  , Output Events.Event
  , RawStore
  , Logger
  , Error AppError
  , Reader LogConfig
  , Reader Events.EventBuffer
  , Embed IO
  ]

type Handler = Sem EffR

-- NOTE: this should probably go in the library
runHandler
  :: AppConfig
  -> Handler a
  -> IO a
runHandler AppConfig{logConfig, authTreeDriver, eventBuffer} m = do
  eRes <- runM .
    runReader eventBuffer .
    runReader logConfig .
    runError .
    Logger.evalKatip .
    interpretAuthTreeStore authTreeDriver .
    Events.eval .
    SimpleStorage.eval $ m
  case eRes of
    Left e  -> throwM e
    Right a -> pure a

