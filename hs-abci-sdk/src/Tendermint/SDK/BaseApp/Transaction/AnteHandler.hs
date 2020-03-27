module Tendermint.SDK.BaseApp.Transaction.AnteHandler
  ( AnteHandler
  ) where

import           Data.Monoid                              (Endo)
import           Polysemy                                 (Sem)
import           Tendermint.SDK.BaseApp.Transaction.Types (RoutingTx)

type AnteHandler r = forall msg a. (Endo (RoutingTx msg -> Sem r a))
