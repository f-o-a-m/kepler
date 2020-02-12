module Tendermint.SDK.Application
  ( ModuleList(..)
  , Module(..)
  , HandlersContext(..)
  , defaultCompileToCore
  , AnteHandler(..)
  , baseAppAnteHandler
  , BaseApp
  , createIOApp
  , makeApp
  ) where

import           Polysemy                               (Sem)
import           Tendermint.SDK.Application.AnteHandler
import           Tendermint.SDK.Application.App
import           Tendermint.SDK.Application.Handlers
import           Tendermint.SDK.Application.Module
import           Tendermint.SDK.BaseApp.BaseEffs        (compileToCoreEffs)
import           Tendermint.SDK.BaseApp.CoreEff         (CoreEffs)
import           Tendermint.SDK.BaseApp.Store.IAVLStore (evalStoreEffs)

defaultCompileToCore
  :: forall a.
     Sem (BaseApp CoreEffs) a
  -> Sem CoreEffs a
defaultCompileToCore = compileToCoreEffs . evalStoreEffs
