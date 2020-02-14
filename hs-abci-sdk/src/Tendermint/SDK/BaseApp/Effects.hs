module Tendermint.SDK.BaseApp.Effects
  ( BaseApp
  , defaultCompileToCore
  , defaultCompileToPureCore
  , module Tendermint.SDK.BaseApp.Effects.BaseEffs
  , module Tendermint.SDK.BaseApp.Effects.CoreEffs
  , module Tendermint.SDK.BaseApp.Effects.PureCoreEffs
  ) where

import           Polysemy                                    (Sem)
import           Tendermint.SDK.BaseApp.Effects.BaseEffs
import           Tendermint.SDK.BaseApp.Effects.CoreEffs
import           Tendermint.SDK.BaseApp.Effects.PureCoreEffs
import           Tendermint.SDK.BaseApp.Store                (StoreEffs)
import qualified Tendermint.SDK.BaseApp.Store.IAVLStore      as IAVL
import qualified Tendermint.SDK.BaseApp.Store.MemoryStore    as Memory
import           Tendermint.SDK.Types.Effects                ((:&))


type BaseApp core = StoreEffs :& BaseEffs :& core

defaultCompileToCore
  :: forall a.
     Sem (BaseApp CoreEffs) a
  -> Sem CoreEffs a
defaultCompileToCore = compileToCore . IAVL.evalStoreEffs

defaultCompileToPureCore
  :: forall a.
     Sem (BaseApp PureCoreEffs) a
  -> Sem PureCoreEffs a
defaultCompileToPureCore = compileToPureCore . Memory.evalStoreEffs
