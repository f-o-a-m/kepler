module Main where

import           Control.Exception                   (bracket)
import           Nameservice.Application             (makeAppConfig)
import           Nameservice.Server                  (makeAndServeApplication)
--import           System.IO                           (stdout)
import           Tendermint.SDK.BaseApp.Logger.Katip (LogConfig (..),
                                                      mkLogConfig)
import Katip
import Katip.Scribes.ElasticSearch
import Database.V5.Bloodhound
import Network.HTTP.Client

main :: IO ()
main = do
  logCfg <- mkLogConfig "dev" "nameservice"
  mgr <- newManager defaultManagerSettings
  let bhe = mkBHEnv (Server "http://localhost:9201") mgr
  esScribe <- mkEsScribe
    -- Reasonable for production
    defaultEsScribeCfgV5
    -- Reasonable for single-node in development
    bhe
    (IndexName "nameservice")
    (MappingName "application-logs")
    (permitItem InfoS)
    V3
  let mkLogEnv = registerScribe "es" esScribe defaultScribeSettings
                 =<< initLogEnv "ABCI" "production"
  bracket mkLogEnv closeScribes $ \le -> do
    cfg <- makeAppConfig logCfg {_logEnv = le}
    makeAndServeApplication cfg

  -- logCfg <- mkLogConfig "dev" "nameservice"
  -- handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  -- let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  -- bracket mkLogEnv K.closeScribes $ \le -> do
  --   cfg <- makeAppConfig logCfg {_logEnv = le}
  --   makeAndServeApplication cfg
