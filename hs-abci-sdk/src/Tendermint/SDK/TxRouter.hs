module Tendermint.SDK.TxRouter where

import           Data.ByteString                  (ByteString)
import           Data.Proxy
import           Polysemy                         (Member, Sem)
import           Polysemy.Error                   (Error, throw)
import           Tendermint.SDK.Auth              (AuthError (..))
import           Tendermint.SDK.Crypto            (Secp256k1)
import           Tendermint.SDK.Types.Transaction (RawTransaction, Tx (..),
                                                   parseTx)

type Handler r msg = Tx Secp256k1 msg -> Sem r ()

router
  :: forall r .
     Member (Error AuthError) r
  => Handler r ByteString
  -> RawTransaction
  -> Sem r ()
router h rawTx =
  case parseTx (Proxy @Secp256k1) rawTx of
    Left errMsg -> throw $ TransactionParseError errMsg
    Right tx    -> h tx
