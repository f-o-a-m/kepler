module Tendermint.SDK.Application
  ( ModuleList(..)
  , Module(..)
  , HandlersContext(..)
  , defaultCompileToCore
  , defaultCompileToCorePure
  , AnteHandler(..)
  , baseAppAnteHandler
  , BaseApp
  , createIOApp
  , makeApp
  ) where

import           Polysemy                                 (Sem)
import           Tendermint.SDK.Application.AnteHandler
import           Tendermint.SDK.Application.App
import           Tendermint.SDK.Application.Handlers
import           Tendermint.SDK.Application.Module
import qualified Tendermint.SDK.BaseApp.BaseEffs          as BE
import           Tendermint.SDK.BaseApp.CoreEff           (CoreEffs)
import           Tendermint.SDK.BaseApp.CoreEffPure       (CoreEffsPure)
import           Tendermint.SDK.BaseApp.Store.IAVLStore   (evalStoreEffs)
import qualified Tendermint.SDK.BaseApp.Store.MemoryStore as Memory

defaultCompileToCore
  :: forall a.
     Sem (BaseApp CoreEffs) a
  -> Sem CoreEffs a
defaultCompileToCore = BE.compileToCore . evalStoreEffs

defaultCompileToCorePure
  :: forall a.
     Sem (BaseApp CoreEffsPure) a
  -> Sem CoreEffsPure a
defaultCompileToCorePure = BE.compileToCore . Memory.evalStoreEffs
