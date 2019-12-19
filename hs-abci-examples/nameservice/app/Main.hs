module Main where

import           Control.Exception                   (bracket)
import           Control.Lens                        ((&), (.~))
import qualified Katip                               as K
import           Nameservice.Application             (AppConfig (..),
                                                      addScribesToLogEnv,
                                                      makeAppConfig)
import           Nameservice.Server                  (makeAndServeApplication)
import qualified Tendermint.SDK.BaseApp              as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip as KL

main :: IO ()
main = do
  let withResource cfg@AppConfig{..} = do
        let BaseApp.Context{..} = baseAppContext
            basicLogEnv = KL._logEnv $ contextLogConfig
        logEnvWithWithScribes <- addScribesToLogEnv cfg basicLogEnv
        let logConfigWithScribes = contextLogConfig & KL.logEnv .~ logEnvWithWithScribes
            cfgWithScribes = cfg {baseAppContext = baseAppContext {BaseApp.contextLogConfig = logConfigWithScribes}}
        makeAndServeApplication cfgWithScribes
      close AppConfig{..} =
        let BaseApp.Context{..} = baseAppContext
            logEnv = KL._logEnv $ contextLogConfig
        in K.closeScribes logEnv
  bracket makeAppConfig close withResource
