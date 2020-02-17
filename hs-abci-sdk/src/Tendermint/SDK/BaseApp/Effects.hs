module Tendermint.SDK.BaseApp.Effects
  ( BaseAppEffs
  , AppEffs
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
import           Tendermint.SDK.BaseApp.Transaction          (TxEffs)
import           Tendermint.SDK.Types.Effects                ((:&))




type BaseAppEffs core = StoreEffs :& BaseEffs :& core

defaultCompileToCore
  :: forall a.
     Sem (BaseAppEffs CoreEffs) a
  -> Sem CoreEffs a
defaultCompileToCore = evalBaseEffs . IAVL.evalStoreEffs

defaultCompileToPureCore
  :: forall a.
     Sem (BaseAppEffs PureCoreEffs) a
  -> Sem PureCoreEffs a
defaultCompileToPureCore = evalBaseEffsPure . Memory.evalStoreEffs

type AppEffs es core = es :& TxEffs :& BaseAppEffs core
