{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Store.Scope
  ( ConnectionScope(..)
  , ApplyScope
  , applyScope
  , ResolveScope(..)
  , MergeScopes(..)
  , mergeScopes
  ) where

import           Polysemy                              (EffectRow, Sem, makeSem,
                                                        rewrite)
import           Polysemy.Tagged                       (Tagged (..))
import           Tendermint.SDK.BaseApp.Store.RawStore (RawStore)

data ConnectionScope = Query | Mempool | Consensus

type family ApplyScope (s :: ConnectionScope) (es :: EffectRow) :: EffectRow where
  ApplyScope s (RawStore ': as) = Tagged s RawStore ': as
  ApplyScope s (a ': as) = a ': ApplyScope s as

applyScope
  :: forall s r.
     forall a. Sem (RawStore ': r) a -> Sem (Tagged s RawStore ': r) a
applyScope = rewrite (Tagged @s)

class ResolveScope s r where
  resolveScope :: Sem (Tagged s RawStore ': r) a -> Sem r a

data MergeScopes m a where
  MergeScopes :: MergeScopes m ()

makeSem ''MergeScopes
