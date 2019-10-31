module Main where

import           Control.Exception
import qualified Katip                     as K
import           SimpleStorage.App         (makeAndServeApplication)
import           SimpleStorage.Application
import           System.IO                 (stdout)
import           Tendermint.SDK.Logger


main :: IO ()
main = do
  logCfg <- mkLogConfig "dev" "simple-storage"
  handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  bracket mkLogEnv K.closeScribes $ \le -> do
    cfg <- makeAppConfig logCfg {_logEnv = le}
    makeAndServeApplication cfg
