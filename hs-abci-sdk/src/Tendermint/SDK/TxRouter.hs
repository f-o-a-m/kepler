module Tendermint.SDK.TxRouter where

import           Polysemy              (Members, Sem)
import           Polysemy.Error        (Error)
import           Tendermint.SDK.Auth   (AuthError, IsMessage, Tx, parseTx, Transaction)
import           Tendermint.SDK.Errors (AppError)

type Handler r msg = Tx msg -> Sem r ()

router
  :: forall msg r.
     Members [Error AppError, Error AuthError] r
  => IsMessage msg
  => Handler r msg
  -> Transaction
  -> Sem r ()
router h tx = parseTx tx >>= h
