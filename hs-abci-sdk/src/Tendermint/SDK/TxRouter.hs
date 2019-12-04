{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.TxRouter
  ( router
  , Router(..)
  ) where

import           Crypto.Hash                      (Digest)
import           Crypto.Hash.Algorithms           (SHA256)
import           Data.ByteString                  (ByteString)
import           Data.Proxy
import           Data.String.Conversions          (cs)
import           Data.Validation                  (Validation (..))
import           GHC.TypeLits                     (KnownSymbol, symbolVal)
import           Polysemy                         (Member, Sem)
import           Polysemy.Error                   (Error, throw)
import           Tendermint.SDK.Auth              (AuthError (..))
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Crypto            (RecoverableSignatureSchema,
                                                   SignatureSchema (..))
import           Tendermint.SDK.Errors            (AppError, SDKError (..),
                                                   throwSDKError)
import           Tendermint.SDK.Module
import           Tendermint.SDK.Types.Message     (Msg (..),
                                                   ValidateMessage (..),
                                                   formatMessageSemanticError)
-- import           Tendermint.SDK.Types.Message     (Msg (..))
import           Tendermint.SDK.Types.Transaction (RawTransaction (..),
                                                   RoutedTx (..), Tx (..),
                                                   parseTx)

router
  :: forall alg ms r .
     Member (Error AuthError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => Router ms r
  => Proxy alg
  -> Modules ms r
  -> RawTransaction
  -> Sem r ()
router (p  :: Proxy alg) ms rawTx =
  case parseTx p rawTx of
    Left errMsg -> throw $ TransactionParseError errMsg
    Right tx    -> route ms tx

class Router ms r where
  route :: forall alg. Modules ms r -> Tx alg ByteString -> Sem r ()

instance (Member (Error AppError) r) => Router '[] r where
  route NilModules Tx{txRoute}  =
    throwSDKError $ UnmatchedRoute txRoute

instance (Member (Error AppError) r, Router ms r, HasCodec msg, ValidateMessage msg, KnownSymbol name) => Router (Module name msg api r ': ms) r where
  route (ConsModule m rest) tx@Tx{..}
    | symbolVal (Proxy :: Proxy name) == cs txRoute = do
        msg <- case decode $ msgData txMsg of
          Left err           -> throwSDKError $ ParseError err
          Right (msg :: msg) -> return msg
        let msg' = txMsg {msgData = msg}
            tx' = RoutedTx $ tx {txMsg = msg'}
        case validateMessage msg' of
          Failure err ->
            throwSDKError . MessageValidation . map formatMessageSemanticError $ err
          Success _ -> pure ()
        moduleRouter m tx'
    | otherwise = route rest tx
