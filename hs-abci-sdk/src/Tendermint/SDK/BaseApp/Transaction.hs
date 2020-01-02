module Tendermint.SDK.BaseApp.Transaction
  ( TxEffs
  , TransactionContext(..)
  , newTransactionContext
  , eval
  ) where

import           Control.Lens                     ((&), (.~))
import qualified Data.ByteArray.Base64String      as Base64
import           Data.Default.Class               (def)
import           Polysemy                         (Sem, raiseUnder)
import           Polysemy.Error                   (Error, runError)
import           Polysemy.Output                  (Output,
                                                   runOutputMonoidAssocR)
import           Polysemy.State                   (State, runState)
import           Tendermint.SDK.BaseApp.Errors    (AppError, txResultAppError)
import qualified Tendermint.SDK.BaseApp.Events    as E
import qualified Tendermint.SDK.BaseApp.Gas       as G
import           Tendermint.SDK.Codec             (HasCodec (encode))
import           Tendermint.SDK.Types.Effects     ((:&))
import           Tendermint.SDK.Types.Transaction (RoutedTx (..), Tx (..))
import           Tendermint.SDK.Types.TxResult    (TxResult, txResultData,
                                                   txResultEvents,
                                                   txResultGasUsed,
                                                   txResultGasWanted)

type TxEffs =
    [ Output E.Event
    , G.GasMeter
    , Error AppError
    ]

data TransactionContext = TransactionContext
  { initialGas :: G.GasAmount
  }

newTransactionContext
  :: RoutedTx msg
  -> TransactionContext
newTransactionContext (RoutedTx Tx{txGas}) = do
  TransactionContext
    { initialGas = G.GasAmount txGas
    }

eval
  :: forall r a.
     HasCodec a
  => TransactionContext
  -> Sem (TxEffs :& r) a
  -> Sem r TxResult
eval TransactionContext{..} action = do
  eRes <-
    runError .
      runState initialGas .
      G.eval .
      raiseUnder @(State G.GasAmount) $
      runOutputMonoidAssocR pure action
  return $ case eRes of
    Left e -> def & txResultAppError .~ e
    Right (gasRemaining, (events, a)) ->
      let gasUsed = initialGas - gasRemaining
      in def & txResultEvents .~ events
             & txResultData .~ Base64.fromBytes (encode a)
             & txResultGasWanted .~ G.unGasAmount initialGas
             & txResultGasUsed .~ G.unGasAmount gasUsed
