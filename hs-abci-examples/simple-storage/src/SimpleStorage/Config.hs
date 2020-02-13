{-# LANGUAGE TemplateHaskell #-}

module SimpleStorage.Config
  ( AppConfig(..)
  , baseAppContext
  , makeAppConfig
  ) where

import           Control.Lens                           (makeLenses, (&), (.~),
                                                         (^.))
import           Data.Maybe                             (fromMaybe)
import           Data.String.Conversions                (cs)
import qualified Katip                                  as K
import           System.Environment
import           System.IO                              (stdout)
import qualified Tendermint.SDK.BaseApp                 as BaseApp
import           Tendermint.SDK.BaseApp.Logger.Katip    as KL
import           Tendermint.SDK.BaseApp.Store.IAVLStore (initIAVLVersions, GrpcConfig(..))


data AppConfig = AppConfig
  { _baseAppContext           :: BaseApp.Context
  }
makeLenses ''AppConfig

makeAppConfig :: IO AppConfig
makeAppConfig = do
  versions <- initIAVLVersions
  grpcConfig <- do
    host <- getEnv "IAVL_HOST"
    port <- read <$> getEnv "IAVL_PORT"
    pure $ GrpcConfig host port
  c <- BaseApp.makeContext (KL.InitialLogNamespace "dev" "simple-storage") Nothing versions grpcConfig
  addScribesToLogEnv $
    AppConfig { _baseAppContext = c
              }

addScribesToLogEnv :: AppConfig -> IO AppConfig
addScribesToLogEnv cfg = do
  logLevel <- makeLogLevel
  let initialLogEnv =  cfg ^. baseAppContext . BaseApp.contextLogConfig  . KL.logEnv
  scribesLogEnv <- makeKatipScribe logLevel initialLogEnv
  pure $ cfg &
    baseAppContext . BaseApp.contextLogConfig  . KL.logEnv .~ scribesLogEnv

--------------------------------------------------------------------------------

data LogLevel = LogLevel
  { severity  :: K.Severity
  , verbosity :: K.Verbosity
  }

makeLogLevel :: IO LogLevel
makeLogLevel = do
  -- LOG_SEVERITY should be in {debug, info, notice, warning, error, critical, alert, emergency}
  msev <- lookupEnv "LOG_SEVERITY"
  let s = fromMaybe K.InfoS (parseSeverity =<< msev)
  -- LOG_VERBOSITY should be in {0,1,2,3}
  mverb <-  lookupEnv "LOG_VERBOSITY"
  let v = fromMaybe K.V0 (parseVerbosity =<< mverb)
  return LogLevel {severity = s, verbosity = v}
  where
    parseSeverity = K.textToSeverity . cs
    parseVerbosity v
      | v == "0" = Just K.V0
      | v == "1" = Just K.V1
      | v == "2" = Just K.V2
      | v == "3" = Just K.V3
      | otherwise = Nothing

-- makes a log environment for console logs / ES logs
makeKatipScribe
  :: LogLevel
  -> K.LogEnv
  -> IO K.LogEnv
makeKatipScribe LogLevel{..} le = do
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem severity) verbosity
  K.registerScribe "stdout" handleScribe K.defaultScribeSettings le
