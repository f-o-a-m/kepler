{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.Application.Module
  ( Module(..)
  , Modules(..)
  , QueryRouter(Api)
  , queryRouter
  , TxRouter
  , txRouter
  ) where

import           Crypto.Hash                      (Digest)
import           Crypto.Hash.Algorithms           (SHA256)
import           Data.ByteString                  (ByteString)
import           Data.Proxy
import           Data.String.Conversions          (cs)
import           Data.Validation                  (Validation (..))
import           GHC.TypeLits                     (KnownSymbol, Symbol,
                                                   symbolVal)
import           Polysemy                         (EffectRow, Member, Sem)
import           Polysemy.Error                   (Error, throw)
import           Servant.API                      ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Errors    (AppError, SDKError (..),
                                                   throwSDKError)
import qualified Tendermint.SDK.BaseApp.Query     as Q
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Crypto            (RecoverableSignatureSchema,
                                                   SignatureSchema (..))
import           Tendermint.SDK.Modules.Auth      (AuthError (..))
import           Tendermint.SDK.Types.Message     (Msg (..),
                                                   ValidateMessage (..),
                                                   formatMessageSemanticError)
import           Tendermint.SDK.Types.Transaction (RoutedTx (..), Tx (..),
                                                   parseTx)

data Module (name :: Symbol) msg (api :: *) (r :: EffectRow) = Module
  { moduleRouter      :: RoutedTx msg -> Sem r ()
  , moduleQueryServer :: Q.RouteT api (Sem r)
  }

data Modules (ms :: [*]) r where
    NilModules :: Modules '[] r
    ConsModule :: Module name msg api r -> Modules ms r -> Modules (Module name msg api r  ': ms) r

--------------------------------------------------------------------------------

queryRouter
  :: QueryRouter ms r
  => Q.HasRouter (Api ms)
  => Modules ms r
  -> Q.QueryApplication (Sem r)
queryRouter (ms :: Modules ms r) = Q.serve (Proxy :: Proxy (Api ms)) (routeQuery ms)

class QueryRouter ms r where
    type Api ms :: *
    routeQuery :: Modules ms r -> Q.RouteT (Api ms) (Sem r)

instance QueryRouter (Module name msg api r ': '[]) r where
    type Api (Module name msg api r ': '[]) = name :> api
    routeQuery (ConsModule m NilModules) = moduleQueryServer m

instance QueryRouter (m' ': ms) r => QueryRouter (Module name msg api r ': m' ': ms) r where
    type Api (Module name msg api r ': m' ': ms) = (name :> api) :<|> Api (m' ': ms)
    routeQuery (ConsModule m rest) = moduleQueryServer m :<|> routeQuery rest

--------------------------------------------------------------------------------

txRouter
  :: forall alg ms r .
     Member (Error AuthError) r
  => RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => TxRouter ms r
  => Proxy alg
  -> Modules ms r
  -> ByteString
  -> Sem r ()
txRouter (p  :: Proxy alg) ms bs =
  let etx = decode bs >>= parseTx p
  in case etx of
       Left errMsg -> throw $ TransactionParseError errMsg
       Right tx    -> routeTx ms tx

class TxRouter ms r where
  routeTx :: forall alg. Modules ms r -> Tx alg ByteString -> Sem r ()

instance (Member (Error AppError) r) => TxRouter '[] r where
  routeTx NilModules Tx{txRoute}  =
    throwSDKError $ UnmatchedRoute txRoute

instance (Member (Error AppError) r, TxRouter ms r, HasCodec msg, ValidateMessage msg, KnownSymbol name) => TxRouter (Module name msg api r ': ms) r where
  routeTx (ConsModule m rest) tx@Tx{..}
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
    | otherwise = routeTx rest tx
