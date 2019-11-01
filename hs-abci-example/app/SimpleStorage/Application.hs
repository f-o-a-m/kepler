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
import           Polysemy                            (Embed, Sem, runM)
import           Polysemy.Error                      (Error, runError)
import           Polysemy.Output                     (Output)
import           Polysemy.Reader                     (Reader, runReader)
import           Polysemy.Resource                   (Resource, resourceToIO)
import           SimpleStorage.Modules.SimpleStorage as SimpleStorage
import           Tendermint.SDK.AuthTreeStore        (AuthTreeDriver,
                                                      initAuthTreeDriver,
                                                      interpretAuthTreeStore)
import qualified Tendermint.SDK.Events               as Events
import qualified Tendermint.SDK.Logger               as Logger
import qualified Tendermint.SDK.Store                as Store

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
  , Error AppError
  , Output Events.Event
  , Store.RawStore
  , Logger.Logger
  , Reader Logger.LogConfig
  , Reader Events.EventBuffer
  , Resource
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
    resourceToIO .
    runReader eventBuffer .
    runReader logConfig .
    Logger.evalKatip .
    interpretAuthTreeStore authTreeDriver .
    Events.eval .
    runError .
    SimpleStorage.eval $ m
  case eRes of
    Left e  -> throwM e
    Right a -> pure a

