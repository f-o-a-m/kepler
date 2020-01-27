module Tendermint.SDK.BaseApp.Transaction.Modifier
  ( OnCheck(..)
  , OnCheckReturn
  ) where

import           Tendermint.SDK.BaseApp.Transaction.Types

type family OnCheckReturn (ctx :: RouteContext) (oc :: OnCheck) a where
    OnCheckReturn 'CheckTx 'OnCheckEval a = a
    OnCheckReturn 'CheckTx 'OnCheckUnit a = ()
    OnCheckReturn 'DeliverTx _ a = a
