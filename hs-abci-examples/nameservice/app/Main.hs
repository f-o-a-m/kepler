module Main where

import           Control.Concurrent                  (killThread)
import           Control.Exception                   (bracket)
import           Control.Lens                        ((^.))
import           Data.IORef                          (readIORef)
import qualified Katip                               as K
import           Nameservice.Config                  (baseAppContext,
                                                      makeAppConfig,
                                                      prometheusServerThreadId)
import           Nameservice.Server                  (makeAndServeApplication)
import qualified Tendermint.SDK.BaseApp              as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip as KL

main :: IO ()
main =
  let close cfg = do
        _ <- K.closeScribes (cfg ^. baseAppContext . BaseApp.contextLogConfig . KL.logEnv)
        prometheusThreadId <- readIORef $ cfg ^. prometheusServerThreadId
        maybe (pure ()) killThread prometheusThreadId
  in bracket makeAppConfig close makeAndServeApplication
