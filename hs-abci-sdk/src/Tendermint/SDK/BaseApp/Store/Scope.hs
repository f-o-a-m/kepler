module Tendermint.SDK.BaseApp.Store.Scope
  ( ConnectionScope(..)
  , ApplyScope
  , applyScope
  , Version(..)
  ) where

import           Numeric.Natural                       (Natural)
import           Polysemy                              (Effect, EffectRow, Sem,
                                                        rewrite)
import           Polysemy.Tagged                       (Tagged(..))

data ConnectionScope = Query | Mempool | Consensus

type family ApplyScope (s :: ConnectionScope) (e :: Effect) (es :: EffectRow) :: EffectRow where
  ApplyScope s e (e ': as) = Tagged s e ': as
  ApplyScope s e (a ': as) = a ': ApplyScope s e as

applyScope
  :: forall (s :: ConnectionScope) e r.
     forall a. Sem (e ': r) a -> Sem (Tagged s e ': r) a
applyScope = rewrite (Tagged @s)

data Version = 
    Genesis
  | Version Natural
  | Latest
