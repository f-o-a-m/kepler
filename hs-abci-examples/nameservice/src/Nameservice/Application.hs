{-# LANGUAGE UndecidableInstances #-}

module Nameservice.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , runHandler
  ) where

import           Control.Exception           (Exception)
import           Control.Monad.Catch         (throwM)
import           Polysemy                    (Sem)
import           Polysemy.Error              (Error, runError)
import qualified Tendermint.SDK.BaseApp      as BaseApp
import qualified Tendermint.SDK.Logger.Katip as KL

data AppConfig = AppConfig
  { baseAppContext :: BaseApp.Context
  }

makeAppConfig :: KL.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  c <- BaseApp.makeContext logCfg
  pure $ AppConfig { baseAppContext = c
                   }

--------------------------------------------------------------------------------

data AppError = AppError String deriving (Show)

instance Exception AppError

type EffR =
  (  Error AppError
  ': BaseApp.BaseApp
  )

type Handler = Sem EffR

-- NOTE: this should probably go in the library
runHandler
  :: AppConfig
  -> Handler a
  -> IO a
runHandler AppConfig{baseAppContext} m = do
  eRes <- BaseApp.eval baseAppContext .
    runError $ m
  case eRes of
    Left e  -> throwM e
    Right a -> pure a
