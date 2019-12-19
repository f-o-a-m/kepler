module Main where

import           Control.Exception                   (bracket)
import qualified Katip                               as K
import           SimpleStorage.Application           (AppConfig (..),
                                                      makeAppConfig)
import           SimpleStorage.Server                (makeAndServeApplication)
import           System.IO                           (stdout)
import qualified Tendermint.SDK.BaseApp              as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip as KL

main :: IO ()
main = do
  cfg@AppConfig{..} <- makeAppConfig -- logCfg {_logEnv = le}
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let BaseApp.Context{..} = baseAppContext
      mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings
        (KL._logEnv contextLogConfig)
  bracket mkLogEnv K.closeScribes $ \le -> do
    let newLogConfig = contextLogConfig { KL._logEnv = le }
        newContext = baseAppContext { BaseApp.contextLogConfig = newLogConfig }
        newCfg = cfg { baseAppContext = newContext }
    makeAndServeApplication newCfg
