module Tendermint.SDK.BaseApp.Transaction.Effect
  ( TxEffs
  , TransactionContext(..)
  , newTransactionContext
  , eval
  ) where

import           Control.Lens                     ((&), (.~))
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteArray.Base64String      as Base64
import           Data.Default.Class               (def)
import           Data.IORef                       (IORef, newIORef, readIORef)
import           Polysemy                         (Embed, Member, Sem,
                                                   raiseUnder)
import           Polysemy.Error                   (Error, runError)
import           Polysemy.Output                  (Output,
                                                   runOutputMonoidAssocR)
import           Polysemy.State                   (State, runStateIORef)
import           Tendermint.SDK.BaseApp.Errors    (AppError, txResultAppError)
import qualified Tendermint.SDK.BaseApp.Events    as E
import qualified Tendermint.SDK.BaseApp.Gas       as G
import           Tendermint.SDK.Codec             (HasCodec (encode))
import           Tendermint.SDK.Types.Effects     ((:&))
import           Tendermint.SDK.Types.Transaction (PreRoutedTx (..), Tx (..))
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
  { gas :: IORef G.GasAmount
  }

newTransactionContext
  :: PreRoutedTx msg
  -> IO TransactionContext
newTransactionContext (PreRoutedTx Tx{txGas}) = do
  initialGas <- newIORef $ G.GasAmount txGas
  pure TransactionContext
    { gas = initialGas
    }

eval
  :: forall r a.
     HasCodec a
  => Member (Embed IO) r
  => TransactionContext
  -> Sem (TxEffs :& r) a
  -> Sem r TxResult
eval TransactionContext{..} action = do
  initialGas <- liftIO $ readIORef gas
  eRes <-
    runError .
      runStateIORef gas .
      G.eval .
      raiseUnder @(State G.GasAmount) $
      runOutputMonoidAssocR (pure @[]) action
  gasRemaining <- liftIO $ readIORef gas
  let gasUsed = initialGas - gasRemaining
      baseResponse =
        def & txResultGasWanted .~ G.unGasAmount initialGas
            & txResultGasUsed .~ G.unGasAmount gasUsed
  return $ case eRes of
    Left e ->
      baseResponse & txResultAppError .~ e
    Right (events, a) ->
      baseResponse & txResultEvents .~ events
                   & txResultData .~ Base64.fromBytes (encode a)
