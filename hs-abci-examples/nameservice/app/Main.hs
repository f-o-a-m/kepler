module Main where

import           Control.Exception                         (bracket)
import           Data.Text                                 (pack)
import qualified Katip                                     as K
import           Nameservice.Application                   (makeAppConfig)
import           Nameservice.Server                        (makeAndServeApplication)
import           System.Environment                        (lookupEnv)
import           System.IO                                 (stdout)
import           Tendermint.SDK.BaseApp.Logger.Katip       (LogConfig (..),
                                                            mkLogConfig)
import           Tendermint.SDK.BaseApp.Metrics.Prometheus (MetricsConfig (..),
                                                            mkMetricsConfig)
import qualified Text.Read                                 as T
import qualified Katip.Scribes.ElasticSearch as ES
import qualified Database.V5.Bloodhound as BH
import qualified Network.HTTP.Client as Client

main :: IO ()
main = do
  -- | Env vars
  mApiKey <- lookupEnv "DD_API_KEY"
  mMetricsPort <- lookupEnv "STATS_PORT"
  mESPort <- lookupEnv "ES_PORT"
  -- | Cfgs
  metCfg <- mkMetricsConfig
  logCfg <- mkLogConfig "dev" "nameservice"
  -- | Log Environment
  mkLogEnv <- case mESPort of
    Nothing -> do
      -- | Console logger context
      handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
      return $ K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
    Just esPort -> do
      -- | ES logger context
      mgr <- Client.newManager Client.defaultManagerSettings
      let serverAddress = "http://localhost:" ++ esPort
          bhe = BH.mkBHEnv (BH.Server $ pack serverAddress) mgr
      esScribe <- ES.mkEsScribe
        -- Reasonable for production
        ES.defaultEsScribeCfgV5
        -- Reasonable for single-node in development
        bhe
        (BH.IndexName "nameservice")
        (BH.MappingName "application-logs")
        (K.permitItem K.InfoS)
        K.V3
      return $ K.registerScribe "es" esScribe K.defaultScribeSettings
              =<< K.initLogEnv "ABCI" "production"
  -- | serve with appropriate log environment
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- makeAppConfig
      metCfg { metricsAPIKey = pack <$> mApiKey
             , metricsPort = T.read <$> mMetricsPort
             }
      logCfg {_logEnv = le}
    makeAndServeApplication cfg
