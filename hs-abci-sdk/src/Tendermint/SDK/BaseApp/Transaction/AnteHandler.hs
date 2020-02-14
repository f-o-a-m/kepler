module Tendermint.SDK.BaseApp.Transaction.AnteHandler
  ( AnteHandler(..)
  ) where

import           Polysemy                                 (Sem)
import           Tendermint.SDK.BaseApp.Transaction.Types (RoutingTx)

newtype AnteHandler r = AnteHandler
  (forall msg a. (RoutingTx msg -> Sem r a) -> (RoutingTx msg -> Sem r a))

instance Semigroup (AnteHandler r) where
  (<>) (AnteHandler h1) (AnteHandler h2) =
      AnteHandler $ h1 . h2

instance Monoid (AnteHandler r) where
  mempty = AnteHandler id
