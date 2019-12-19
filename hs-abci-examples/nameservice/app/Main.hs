module Main where

import           Control.Exception                         (bracket)
import           Data.Text                                 (pack)
import qualified Database.V5.Bloodhound                    as BH
import qualified Katip                                     as K
import qualified Katip.Scribes.ElasticSearch               as ES
import           Nameservice.Application                   (AppConfig (..),
                                                            addScribesToLogEnv,
                                                            makeAppConfig)
import           Nameservice.Server                        (makeAndServeApplication)
import qualified Network.HTTP.Client                       as Client
import           System.Environment                        (lookupEnv)
import           System.IO                                 (stdout)
import qualified Tendermint.SDK.BaseApp                    as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import           Tendermint.SDK.BaseApp.Metrics.Prometheus (PrometheusConfig (..),
                                                            mkPrometheusConfig)
import qualified Text.Read                                 as T

main :: IO ()
main = do
  -- Env vars
  mPrometheusPort <- lookupEnv "STATS_PORT"
  -- Cfgs
  metCfg <- mkPrometheusConfig
  -- logCfg <- mkLogConfig "dev" "nameservice"
  -- Log Environment
  bracket mkLogEnv K.closeScribes $ \le -> do
    leWithScribes <- addScribesToLogEnv le
    cfg@AppConfig{..} <- makeAppConfig
      metCfg { metricsPort = T.read <$> mPrometheusPort }
      -- logCfg { _logEnv = leWithScribes }
    let BaseApp.Context{..} = baseAppContext
        logCfg = contextLogConfig { KL._logEnv = le }
        newBAContext = baseAppContext { BaseApp.contextLogConfig = logCfg }
        newCfg = cfg { baseAppContext = newBAContext }
    makeAndServeApplication cfg
