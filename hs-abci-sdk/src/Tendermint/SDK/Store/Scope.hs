{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Store.Scope
  ( ConnectionScope(..)
  , ApplyScope
  , applyScope
  , MergeScopes(..)
  , mergeScopes
  ) where

import           Polysemy                      (EffectRow, Sem, makeSem,
                                                rewrite)
import           Polysemy.Tagged               (Tagged (..))
import           Tendermint.SDK.Store.RawStore (RawStore)

data ConnectionScope = Query | Mempool | Consensus

type family ApplyScope (s :: ConnectionScope) (es :: EffectRow) :: EffectRow where
  ApplyScope s (RawStore ': as) = Tagged s RawStore ': as
  ApplyScope s (a ': as) = a ': ApplyScope s as

applyScope
  :: forall s r.
     forall a. Sem (RawStore ': r) a -> Sem (Tagged s RawStore ': r) a
applyScope = rewrite (Tagged @s)

data MergeScopes m a where
  MergeScopes :: MergeScopes m ()

makeSem ''MergeScopes
