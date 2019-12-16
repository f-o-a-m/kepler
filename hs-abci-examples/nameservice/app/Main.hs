module Main where

import           Control.Exception                         (bracket)
import qualified Katip                                     as K
import           Nameservice.Application                   (makeAppConfig)
import           Nameservice.Server                        (makeAndServeApplication)
import           System.IO                                 (stdout)
import           Tendermint.SDK.BaseApp.Logger.Katip       (LogConfig (..),
                                                            mkLogConfig)
import           Tendermint.SDK.BaseApp.Metrics.Prometheus (MetricsConfig (..),
                                                            mkMetricsConfig)

main :: IO ()
main = do
  metCfg <- mkMetricsConfig
  logCfg <- mkLogConfig "dev" "nameservice"
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- makeAppConfig metCfg logCfg {_logEnv = le}
    makeAndServeApplication cfg
