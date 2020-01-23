module Tendermint.SDK.BaseApp.Transaction.Types
  ( TransactionApplication
  , PreRoutedTx(..)
  , OnCheck(..)
  , RouteContext(..)
  , Return'
  , Return
  , (:~>)
  , TypedMessage
  -- * Re-Exports
  , Tx(..)
  ) where

import           Control.Lens                     (lens)
import           Data.ByteString                  (ByteString)
import           GHC.TypeLits                     (Symbol)
import           Tendermint.SDK.BaseApp.Router    (HasPath (..))
import           Tendermint.SDK.Types.Transaction (Tx (..))
import           Tendermint.SDK.Types.TxResult    (TxResult)

data msg :~> a

data TypedMessage (t :: Symbol) msg

data OnCheck = OnCheckEval | OnCheckUnit

data Return' (c :: OnCheck) a

type Return = Return' 'OnCheckUnit

data RouteContext = CheckTx | DeliverTx

type TransactionApplication m = PreRoutedTx ByteString -> m TxResult

data PreRoutedTx msg where
  PreRoutedTx :: Tx alg msg -> PreRoutedTx msg

instance Functor PreRoutedTx where
  fmap f (PreRoutedTx tx) = PreRoutedTx $ fmap f tx

instance HasPath (PreRoutedTx msg) where
  path = lens (\(PreRoutedTx tx) -> txRoute tx)
    (\(PreRoutedTx tx) r -> PreRoutedTx tx {txRoute = r})
