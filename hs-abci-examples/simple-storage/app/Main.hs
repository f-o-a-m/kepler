module Main where

import           Control.Exception                   (bracket)
import           Control.Lens                        ((^.))
import qualified Katip                               as K
import           SimpleStorage.Config                (baseAppContext,
                                                      makeAppConfig)
import           SimpleStorage.Server                (makeAndServeApplication)
import qualified Tendermint.SDK.BaseApp              as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip as KL

main :: IO ()
main =
  let close cfg = K.closeScribes (cfg ^. baseAppContext . BaseApp.contextLogConfig . KL.logEnv)
  in bracket makeAppConfig close makeAndServeApplication

