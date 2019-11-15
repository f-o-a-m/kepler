module Tendermint.SDK.TxRouter where

import           Polysemy              (Members, Sem)
import           Polysemy.Error        (Error, throw)
import           Tendermint.SDK.Auth   (AuthError, IsMessage (..), Transaction,
                                        Tx (..), formatWireParseError, parseTx)
import           Tendermint.SDK.Errors (SDKError (..))

type Handler r msg = Tx msg -> Sem r ()

router
  :: forall msg r.
     Members [Error SDKError, Error AuthError] r
  => IsMessage msg
  => Handler r msg
  -> Transaction
  -> Sem r ()
router h tx = do
  eRes <- parseTx tx
  case eRes of
    Left err  -> throw @SDKError (ParseError $ formatWireParseError err)
    Right (res :: Tx msg) -> h res
