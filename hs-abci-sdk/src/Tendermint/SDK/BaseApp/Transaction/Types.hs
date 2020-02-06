{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.BaseApp.Transaction.Types
  ( module Tendermint.SDK.BaseApp.Transaction.Types
  -- * Re-Exports
  , Tx(..)
  ) where

import           Control.Lens                     (lens)
import           Data.ByteString                  (ByteString)
import           Data.Kind                        (Constraint)
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

data SRouteContext (c :: RouteContext) where
  SCheckTx :: SRouteContext 'CheckTx
  SDeliverTx :: SRouteContext 'DeliverTx

type family HasWriteInContext (c :: RouteContext) (r :: EffectRow) :: Constraint where
  HasWriteInContext 'CheckTx _ = ()
  HasWriteInContext 'DeliverTx r = Member WriteStore r

type TransactionApplication m = RoutingTx ByteString -> m TxResult

data EmptyTxServer = EmptyTxServer

data RoutingTx msg where
  RoutingTx :: Tx alg msg -> RoutingTx msg

instance Functor RoutingTx where
  fmap f (RoutingTx tx) = RoutingTx $ fmap f tx

instance HasPath (RoutingTx msg) where
  path = lens (\(RoutingTx tx) -> txRoute tx)
    (\(RoutingTx tx) r -> RoutingTx tx {txRoute = r})
