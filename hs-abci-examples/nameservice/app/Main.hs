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

main :: IO ()
main = do
  metCfg <- mkMetricsConfig
  mApiKey <- lookupEnv "DD_API_KEY"
  mMetricsPort <- lookupEnv "STATS_PORT"
  logCfg <- mkLogConfig "dev" "nameservice"
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- makeAppConfig
      metCfg { metricsAPIKey = pack <$> mApiKey
             , metricsPort = T.read <$> mMetricsPort
             }
      logCfg {_logEnv = le}
    makeAndServeApplication cfg
