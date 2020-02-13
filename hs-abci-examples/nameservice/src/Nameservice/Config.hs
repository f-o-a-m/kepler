{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Config
  ( AppConfig(..)
  , baseAppContext
  , prometheusServerThreadId
  , makeAppConfig
  ) where

import           Control.Concurrent                        (ThreadId)
import           Control.Error                             (MaybeT (..),
                                                            runMaybeT)
import           Control.Lens                              (makeLenses, (&),
                                                            (.~), (^.))
import           Data.IORef                                (IORef, newIORef)
import           Data.Maybe                                (fromMaybe)
import           Data.String.Conversions                   (cs)
import qualified Database.V5.Bloodhound                    as BH
import qualified Katip                                     as K
import qualified Katip.Scribes.ElasticSearch               as ES
import qualified Network.HTTP.Client                       as Client
import           System.Environment
import           System.IO                                 (stdout)
import qualified Tendermint.SDK.BaseApp                    as BaseApp
import           Tendermint.SDK.BaseApp.Logger.Katip       as KL
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus as P
import           Tendermint.SDK.BaseApp.Store.IAVLStore    (GrpcConfig (..),
                                                            initIAVLVersions)
import           Text.Read                                 (read)


data AppConfig = AppConfig
  { _baseAppContext           :: BaseApp.Context
  , _prometheusServerThreadId :: IORef (Maybe ThreadId)
  }
makeLenses ''AppConfig

makeAppConfig :: IO AppConfig
makeAppConfig = do
  versions <- initIAVLVersions
  grpcConfig <- do
    host <- getEnv "IAVL_HOST"
    port <- read <$> getEnv "IAVL_PORT"
    pure $ GrpcConfig host port
  prometheusEnv <- runMaybeT $ do
    prometheusPort <- read <$> MaybeT (lookupEnv "STATS_PORT")
    pure $ P.MetricsScrapingConfig prometheusPort
  c <- BaseApp.makeContext (KL.InitialLogNamespace "dev" "nameservice") prometheusEnv versions grpcConfig
  prometheusServer <- newIORef Nothing
  addScribesToLogEnv $
    AppConfig { _baseAppContext = c
              , _prometheusServerThreadId = prometheusServer
              }

addScribesToLogEnv :: AppConfig -> IO AppConfig
addScribesToLogEnv cfg = do
  logLevel <- makeLogLevel
  loggingCfg <- makeLoggingConfig
  let initialLogEnv =  cfg ^. baseAppContext . BaseApp.contextLogConfig  . KL.logEnv
  scribesLogEnv <- makeKatipScribe loggingCfg logLevel initialLogEnv
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


data KatipConfig = ES {host :: String, port :: String} | Console

makeLoggingConfig :: IO KatipConfig
makeLoggingConfig = do
  mEsConfig <- runMaybeT $
    ES <$> (MaybeT $ lookupEnv "ES_HOST") <*> (MaybeT $ lookupEnv "ES_PORT")
  pure $ fromMaybe Console mEsConfig

-- makes a log environment for console logs / ES logs
makeKatipScribe
  :: KatipConfig
  -> LogLevel
  -> K.LogEnv
  -> IO K.LogEnv
makeKatipScribe kcfg LogLevel{..} le = case kcfg of
  Console -> do
    handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem severity) verbosity
    K.registerScribe "stdout" handleScribe K.defaultScribeSettings le
  ES {host, port} -> do
    mgr <- Client.newManager Client.defaultManagerSettings
    let serverAddress = "http://" <> host <> ":" <> port
        bloodhoundEnv = BH.mkBHEnv (BH.Server $ cs serverAddress) mgr
    esScribe <- ES.mkEsScribe ES.defaultEsScribeCfgV5 bloodhoundEnv (BH.IndexName "nameservice")
      (BH.MappingName "application-logs") (K.permitItem severity) verbosity
    K.registerScribe "es" esScribe K.defaultScribeSettings le
