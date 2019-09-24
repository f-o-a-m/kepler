module Main where

import           Control.Exception
import qualified Katip                     as K
import           SimpleStorage.App         (makeAndServeApplication)
import           SimpleStorage.Application
import           SimpleStorage.Logging
import           System.IO                 (stdout)


main :: IO ()
main = do
  logCfg <- mkLogConfig "simple-storage"
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout K.DebugS K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- makeAppConfig logCfg {_logEnv = le}
    makeAndServeApplication cfg
