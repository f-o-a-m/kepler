module Main where

import           Control.Exception                         (bracket)
import qualified Katip                                     as K
import           Nameservice.Application                   (AppConfig (..),
                                                            addScribesToLogEnv,
                                                            makeAppConfig)
import           Nameservice.Server                        (makeAndServeApplication)
import           System.Environment                        (lookupEnv)
import qualified Tendermint.SDK.BaseApp                    as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import           Tendermint.SDK.BaseApp.Metrics.Prometheus (PrometheusConfig (..),
                                                            mkPrometheusConfig)
import qualified Text.Read                                 as T

main :: IO ()
main = do
  mPrometheusPort <- lookupEnv "STATS_PORT"
  metCfg <- mkPrometheusConfig
  -- Log Environment
  cfg@AppConfig{..} <- makeAppConfig
    metCfg { metricsPort = T.read <$> mPrometheusPort }
  let BaseApp.Context{..} = baseAppContext
      mkLogEnv = return . KL._logEnv $ contextLogConfig
  bracket mkLogEnv K.closeScribes $ \le -> do
    leWithScribes <- addScribesToLogEnv cfg le
    let logCfg = contextLogConfig { KL._logEnv = leWithScribes }
        newBAContext = baseAppContext { BaseApp.contextLogConfig = logCfg }
        newCfg = cfg { baseAppContext = newBAContext }
    makeAndServeApplication newCfg
