module Tendermint.SDK.Module where

import           GHC.TypeLits                     (Symbol)
import           Polysemy                         (EffectRow, Sem)
import           Tendermint.SDK.Query             (RouteT)
import           Tendermint.SDK.Types.Transaction (RoutedTx)

data Module (name :: Symbol) msg api (r :: EffectRow) = Module
  { moduleRouter      :: RoutedTx msg -> Sem r ()
  , moduleQueryServer :: RouteT api (Sem r)
  }

data Modules (ms :: [*]) r where
    NilModules :: Modules '[] r
    ConsModule :: Module name msg api r -> Modules ms r -> Modules (Module name msg api r  ': ms) r
