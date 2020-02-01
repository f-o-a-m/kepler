{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.BaseApp.Transaction.Types
  ( module Tendermint.SDK.BaseApp.Transaction.Types
  -- * Re-Exports
  , Tx(..)
  , Sing(SCheckTx, SDeliverTx)
  ) where

import           Control.Lens                     (lens)
import           Data.ByteString                  (ByteString)
import           Data.Kind                        (Constraint)
import           Data.Singletons.TH               (Sing, genSingletons)
import           Polysemy                         (EffectRow, Member)
import           Tendermint.SDK.BaseApp.Router    (HasPath (..))
import           Tendermint.SDK.BaseApp.Store     (WriteStore)
import           Tendermint.SDK.Types.Transaction (Tx (..))
import           Tendermint.SDK.Types.TxResult    (TxResult)

data msg :~> a

data TypedMessage msg

data OnCheck = OnCheckEval | OnCheckUnit

data Return' (c :: OnCheck) a

type Return = Return' 'OnCheckUnit

data RouteContext = CheckTx | DeliverTx deriving (Eq, Show)

$(genSingletons [''RouteContext])


type TransactionApplication m = RoutingTx ByteString -> m TxResult

data EmptyTxServer = EmptyTxServer

data RoutingTx msg where
  RoutingTx :: Tx alg msg -> RoutingTx msg

instance Functor RoutingTx where
  fmap f (RoutingTx tx) = RoutingTx $ fmap f tx

instance HasPath (RoutingTx msg) where
  path = lens (\(RoutingTx tx) -> txRoute tx)
    (\(RoutingTx tx) r -> RoutingTx tx {txRoute = r})

type family StoreDeps (c :: RouteContext) (r :: EffectRow) :: Constraint where
  StoreDeps 'CheckTx r = ()
  StoreDeps 'DeliverTx r = Member WriteStore r
