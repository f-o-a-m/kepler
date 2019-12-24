{-# LANGUAGE TemplateHaskell #-}

module SimpleStorage.Config
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
  c <- BaseApp.makeContext (KL.InitialLogNamespace "dev" "simple-storage") prometheusEnv
  prometheusServer <- newIORef Nothing
  addScribesToLogEnv $
    AppConfig { _baseAppContext = c
              , _prometheusServerThreadId = prometheusServer
              , _serverMetricsMap = metricsMap
              }

addScribesToLogEnv :: AppConfig -> IO AppConfig
addScribesToLogEnv cfg = do
  consoleCfg <- makeConsoleLoggingConfig
  let initialLogEnv =  cfg ^. baseAppContext . BaseApp.contextLogConfig  . KL.logEnv
      mdatadogApiKey =  cfg ^? baseAppContext . BaseApp.contextPrometheusEnv .
        _Just . P.envMetricsScrapingConfig . P.dataDogApiKey
      addScribes = runKleisli $
             makeKatipScribe consoleCfg
         >>> maybe returnA makeMetricsScribe mdatadogApiKey
  scribesLogEnv <- addScribes initialLogEnv
  pure $ cfg &
    baseAppContext . BaseApp.contextLogConfig  . KL.logEnv .~ scribesLogEnv

--------------------------------------------------------------------------------

data KatipConfig = ES {host :: String, port :: String} | Console

makeConsoleLoggingConfig :: IO KatipConfig
makeConsoleLoggingConfig = do
  mEsConfig <- runMaybeT $
    ES <$> (MaybeT $ lookupEnv "ES_HOST") <*> (MaybeT $ lookupEnv "ES_PORT")
  pure $ fromMaybe Console mEsConfig

-- makes a log environment for console logs / ES logs
makeKatipScribe :: KatipConfig -> Kleisli IO K.LogEnv K.LogEnv
makeKatipScribe kcfg = Kleisli $ \le ->
  let verbosity = K.V0
  in case kcfg of
    Console -> do
      handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) verbosity
      K.registerScribe "stdout" handleScribe K.defaultScribeSettings le
    ES {host, port} -> do
      mgr <- Client.newManager Client.defaultManagerSettings
      let serverAddress = "http://" <> host <> ":" <> port
          bloodhoundEnv = BH.mkBHEnv (BH.Server $ cs serverAddress) mgr
      esScribe <- ES.mkEsScribe ES.defaultEsScribeCfgV5 bloodhoundEnv (BH.IndexName "nameservice")
        (BH.MappingName "application-logs") (K.permitItem K.DebugS) verbosity
      K.registerScribe "es" esScribe K.defaultScribeSettings le

--------------------------------------------------------------------------------

-- makes a log environment for metrics logs
makeMetricsScribe :: Text -> Kleisli IO K.LogEnv K.LogEnv
makeMetricsScribe key = Kleisli $ \le -> do
  let apiKey = DD.APIKey key
  datadogScribeSettings <- DD.mkDatadogScribeSettings DD.directAPIConnectionParams (DD.DirectAuth apiKey)
  scribe <- DD.mkDatadogScribe datadogScribeSettings (K.permitItem K.InfoS) K.V0
  K.registerScribe "datadog" scribe K.defaultScribeSettings le
