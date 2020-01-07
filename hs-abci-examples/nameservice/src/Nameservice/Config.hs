{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Config
  ( AppConfig(..)
  , baseAppContext
  , prometheusServerThreadId
  , serverMetricsMap
  , makeAppConfig
  ) where

import           Control.Arrow                                (Kleisli (..),
                                                               returnA, (>>>))
import           Control.Concurrent                           (ThreadId)
import           Control.Concurrent.MVar                      (MVar, newMVar)
import           Control.Error                                (MaybeT (..),
                                                               runMaybeT)
import           Control.Lens                                 (makeLenses, (&),
                                                               (.~), (^.), (^?),
                                                               _Just)
import           Data.IORef                                   (IORef, newIORef)
import           Data.Map.Strict                              (Map, empty)
import           Data.Maybe                                   (fromMaybe)
import           Data.String.Conversions                      (cs)
import           Data.Text                                    (Text)
import qualified Database.V5.Bloodhound                       as BH
import qualified Katip                                        as K
import qualified Katip.Scribes.Datadog.TCP                    as DD
import qualified Katip.Scribes.ElasticSearch                  as ES
import           Network.ABCI.Server.Middleware.MetricsLogger (OrderedMessageType)
import qualified Network.HTTP.Client                          as Client
import           System.Environment
import           System.IO                                    (stdout)
import qualified Tendermint.SDK.BaseApp                       as BaseApp
import           Tendermint.SDK.BaseApp.Logger.Katip          as KL
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus    as P
import           Text.Read                                    (read)



data AppConfig = AppConfig
  { _baseAppContext           :: BaseApp.Context
  , _prometheusServerThreadId :: IORef (Maybe ThreadId)
  , _serverMetricsMap         :: MVar (Map OrderedMessageType Integer)
  }
makeLenses ''AppConfig

makeAppConfig :: IO AppConfig
makeAppConfig = do
  prometheusEnv <- runMaybeT $ do
    prometheusPort <- read <$> MaybeT (lookupEnv "STATS_PORT")
    ddApiKey <- cs <$> MaybeT (lookupEnv "DD_API_KEY")
    pure $ P.MetricsScrapingConfig prometheusPort ddApiKey
  metricsMap <- newMVar empty
  c <- BaseApp.makeContext (KL.InitialLogNamespace "dev" "nameservice") prometheusEnv
  prometheusServer <- newIORef Nothing
  addScribesToLogEnv $
    AppConfig { _baseAppContext = c
              , _prometheusServerThreadId = prometheusServer
              , _serverMetricsMap = metricsMap
              }

addScribesToLogEnv :: AppConfig -> IO AppConfig
addScribesToLogEnv cfg = do
  logLevel <- makeLogLevel
  consoleCfg <- makeConsoleLoggingConfig
  let initialLogEnv =  cfg ^. baseAppContext . BaseApp.contextLogConfig  . KL.logEnv
      mdatadogApiKey =  cfg ^? baseAppContext . BaseApp.contextPrometheusEnv .
        _Just . P.envMetricsScrapingConfig . P.dataDogApiKey
      addScribes = runKleisli $
             makeKatipScribe consoleCfg logLevel
         >>> maybe returnA (makeMetricsScribe logLevel) mdatadogApiKey
  scribesLogEnv <- addScribes initialLogEnv
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

makeConsoleLoggingConfig :: IO KatipConfig
makeConsoleLoggingConfig = do
  mEsConfig <- runMaybeT $
    ES <$> (MaybeT $ lookupEnv "ES_HOST") <*> (MaybeT $ lookupEnv "ES_PORT")
  pure $ fromMaybe Console mEsConfig

-- makes a log environment for console logs / ES logs
makeKatipScribe :: KatipConfig -> LogLevel -> Kleisli IO K.LogEnv K.LogEnv
makeKatipScribe kcfg LogLevel{..} = Kleisli $ \le ->
  case kcfg of
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

--------------------------------------------------------------------------------

-- makes a log environment for metrics logs
makeMetricsScribe :: LogLevel -> Text -> Kleisli IO K.LogEnv K.LogEnv
makeMetricsScribe LogLevel{..} key = Kleisli $ \le -> do
  let apiKey = DD.APIKey key
  datadogScribeSettings <- DD.mkDatadogScribeSettings DD.directAPIConnectionParams (DD.DirectAuth apiKey)
  scribe <- DD.mkDatadogScribe datadogScribeSettings (K.permitItem severity) verbosity
  K.registerScribe "datadog" scribe K.defaultScribeSettings le
