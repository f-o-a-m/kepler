module Main where

import           SimpleStorage.App         (makeAndServeApplication)
import           SimpleStorage.Application
import           System.IO                 (stdout)
import           Tendermint.SDK.Logger
import Katip as K
import Control.Exception (bracket)


main :: IO ()
main = do
  logCfg <- mkLogConfig "dev" "simple-storage"
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- makeAppConfig logCfg {_logEnv = le}
    makeAndServeApplication cfg
