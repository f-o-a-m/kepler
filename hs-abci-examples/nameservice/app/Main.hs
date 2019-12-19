module Main where

import           Control.Exception                         (bracket)
import           Data.Text                                 (pack)
import qualified Database.V5.Bloodhound                    as BH
import qualified Katip                                     as K
import qualified Katip.Scribes.ElasticSearch               as ES
import           Nameservice.Application                   (makeAppConfig)
import           Nameservice.Server                        (makeAndServeApplication)
import qualified Network.HTTP.Client                       as Client
import           System.Environment                        (lookupEnv)
import           System.IO                                 (stdout)
import           Tendermint.SDK.BaseApp.Logger.Katip       (LogConfig (..),
                                                            mkLogConfig)
import           Tendermint.SDK.BaseApp.Metrics.Prometheus (MetricsConfig (..),
                                                            mkMetricsConfig)
import qualified Text.Read                                 as T

main :: IO ()
main = do
  -- Env vars
  mPrometheusPort <- lookupEnv "STATS_PORT"
  -- Cfgs
  metCfg <- mkPrometheusConfig
  logCfg <- mkLogConfig "dev" "nameservice"
  -- Log Environment
  bracket mkLogEnv K.closeScribes $ \le -> do
    leWithScribes <- addScribesToLogEnv le
    cfg <- makeAppConfig
      metCfg { metricsAPIKey = pack <$> mApiKey
             , metricsPort = T.read <$> mMetricsPort
             }
      logCfg { _logEnv = leWithScribes }
    makeAndServeApplication cfg
