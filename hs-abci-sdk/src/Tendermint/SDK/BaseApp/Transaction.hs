module Tendermint.SDK.BaseApp.Transaction
  ( serveTxApplication
    -- * Re-Exports
  , module Tendermint.SDK.BaseApp.Transaction.Types
  , HasTxRouter(..)
  , emptyTxServer
  , DefaultCheckTx(..)
  , VoidReturn
  , TxEffs
  , evalReadOnly
  , evalToBlock
  , AnteHandler
  ) where

import           Control.Lens                                   ((&), (.~))
import           Data.ByteString                                (ByteString)
import           Data.Default.Class                             (def)
import           Data.Proxy
import           Polysemy                                       (Sem)
import           Tendermint.SDK.BaseApp.Errors                  (makeAppError, txResultAppError)
import           Tendermint.SDK.BaseApp.Router                  (Application, RouteResult (..),
                                                                 emptyDelayed,
                                                                 runRouter)
import           Tendermint.SDK.BaseApp.Transaction.AnteHandler
import           Tendermint.SDK.BaseApp.Transaction.Cache       (Cache)
import           Tendermint.SDK.BaseApp.Transaction.Checker
import           Tendermint.SDK.BaseApp.Transaction.Effect
import           Tendermint.SDK.BaseApp.Transaction.Router
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Types.Effects                   ((:&))
import           Tendermint.SDK.Types.TxResult                  (TxResult)

serveTxApplication
  :: HasTxRouter layout r scope
  => Proxy layout
  -> Proxy r
  -> Proxy scope
  -> RouteTx layout (TxEffs :& r)
  -> TransactionApplication (Sem r)
serveTxApplication pl pr ps server =
  toTxApplication (runRouter (routeTx pl pr ps (emptyDelayed (Route server))) ())

toTxApplication
  :: Application (Sem r) (RoutingTx ByteString) (TxResult, Maybe Cache)
  -> TransactionApplication (Sem r)
toTxApplication ra tx = do
  res <- ra tx
  case res of
    Fail e      -> pure (def & txResultAppError .~ makeAppError e, Nothing)
    FailFatal e -> pure (def & txResultAppError .~ makeAppError e, Nothing)
    Route a     -> pure a
