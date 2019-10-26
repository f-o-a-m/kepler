module Main where

import           SimpleStorage.App         (makeAndServeApplication)
import           SimpleStorage.Application
import           Tendermint.SDK.Logger


main :: IO ()
main = do
  logCfg <- mkLogConfig "simple-storage" "SimpleStorage"

  -- | Not sure why we do this here since we create the logCfg already?
  -- handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) K.V2
  -- let mkLogEnv = K.registerScribe "stdout" handleScribe K.defaultScribeSettings (_logEnv logCfg)
  -- bracket mkLogEnv K.closeScribes $ \le -> do
  -- cfg <- makeAppConfig logCfg {_logEnv = le}

  cfg <- makeAppConfig logCfg
  makeAndServeApplication cfg
