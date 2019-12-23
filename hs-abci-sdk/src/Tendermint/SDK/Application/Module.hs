{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.Application.Module
  ( Module(..)
  , voidModuleMessages
  , defaultTxChecker
  , Modules(..)
  , QueryRouter(Api)
  , queryRouter
  , TxRouteContext(..)
  , TxRouter
  , txRouter
  , voidRouter
  , Eval(..)
  ) where

import           Crypto.Hash                      (Digest)
import           Crypto.Hash.Algorithms           (SHA256)
import           Data.ByteString                  (ByteString)
import           Data.Proxy
import           Data.String.Conversions          (cs)
import qualified Data.Validation                  as V
import           Data.Void
import           GHC.TypeLits                     (KnownSymbol, Symbol,
                                                   symbolVal)
import           Polysemy                         (EffectRow, Member, Members,
                                                   Sem)
import           Polysemy.Error                   (Error)
import           Servant.API                      ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp           ((:&), AppError, BaseApp,
                                                   BaseAppEffs, SDKError (..),
                                                   throwSDKError)
import qualified Tendermint.SDK.BaseApp.Query     as Q
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Crypto            (RecoverableSignatureSchema,
                                                   SignatureSchema (..))
import           Tendermint.SDK.Types.Message     (Msg (..),
                                                   ValidateMessage (..),
                                                   formatMessageSemanticError)
import           Tendermint.SDK.Types.Transaction (RoutedTx (..), Tx (..),
                                                   parseTx)

data Module (name :: Symbol) msg (api :: *) (s :: EffectRow) (r :: EffectRow) = Module
  { moduleTxDeliverer :: RoutedTx msg -> Sem r ()
  , moduleTxChecker   :: RoutedTx msg -> Sem r ()
  , moduleQueryServer :: Q.RouteT api (Sem r)
  , moduleEval :: forall deps. Members BaseAppEffs deps => forall a. Sem (s :& deps) a -> Sem deps a
  }

voidModuleMessages :: Module name msg api s r -> Module name Void api s r
voidModuleMessages m =
  m { moduleTxDeliverer = voidRouter
    , moduleTxChecker = voidRouter
    }

defaultTxChecker
  :: Member (Error AppError) r
  => ValidateMessage msg
  => RoutedTx msg
  -> Sem r ()
defaultTxChecker (RoutedTx Tx{txMsg}) =
  case validateMessage txMsg of
    V.Failure err ->
      throwSDKError . MessageValidation . map formatMessageSemanticError $ err
    V.Success _ -> pure ()

data Modules (ms :: [*]) r where
    NilModules :: Modules '[] r
    (:+) :: Module name msg api s r -> Modules ms r -> Modules (Module name msg api s r  ': ms) r

infixr 5 :+

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

instance QueryRouter '[Module name msg api s r] r where
    type Api '[Module name msg api s r] = name :> api
    routeQuery (m :+ NilModules) = moduleQueryServer m

instance QueryRouter (m' ': ms) r => QueryRouter (Module name msg api s r ': m' ': ms) r where
    type Api (Module name msg api s r ': m' ': ms) = (name :> api) :<|> Api (m' ': ms)
    routeQuery (m :+ rest) = moduleQueryServer m :<|> routeQuery rest

--------------------------------------------------------------------------------

data TxRouteContext =
  CheckTxContext | DeliverTxContext

txRouter
  :: forall alg ms r .
     RecoverableSignatureSchema alg
  => Member (Error AppError) r
  => Message alg ~ Digest SHA256
  => TxRouter ms r
  => Proxy alg
  -> TxRouteContext
  -> Modules ms r
  -> ByteString
  -> Sem r ()
txRouter (p  :: Proxy alg) routeContext ms bs =
  let etx = decode bs >>= parseTx p
  in case etx of
       Left errMsg -> throwSDKError $ ParseError ("Transaction ParseError: " <> errMsg)
       Right tx    -> routeTx routeContext ms tx

class TxRouter ms r where
  routeTx :: forall alg. TxRouteContext -> Modules ms r -> Tx alg ByteString -> Sem r ()

instance (Member (Error AppError) r) => TxRouter '[] r where
  routeTx _ NilModules Tx{txRoute}  =
    throwSDKError $ UnmatchedRoute txRoute

instance {-# OVERLAPPING #-} (Member (Error AppError) r, TxRouter ms r,  KnownSymbol name) => TxRouter (Module name Void api s r ': ms) r where
  routeTx routeContext (_ :+ rest) tx@Tx{txRoute}
    | symbolVal (Proxy :: Proxy name) == cs txRoute = throwSDKError $ UnmatchedRoute txRoute
    | otherwise = routeTx routeContext rest tx

instance {-# OVERLAPPABLE #-} (Member (Error AppError) r, TxRouter ms r, HasCodec msg, KnownSymbol name) => TxRouter (Module name msg api s r ': ms) r where
  routeTx routeContext (m :+ rest) tx@Tx{..}
    | symbolVal (Proxy :: Proxy name) == cs txRoute = do
        msg <- case decode $ msgData txMsg of
          Left err           -> throwSDKError $ ParseError err
          Right (msg :: msg) -> return msg
        let msg' = txMsg {msgData = msg}
            tx' = RoutedTx $ tx {txMsg = msg'}
        case routeContext of
          CheckTxContext   -> moduleTxChecker m tx'
          DeliverTxContext -> moduleTxDeliverer m tx'
    | otherwise = routeTx routeContext rest tx

voidRouter
  :: forall a r.
     RoutedTx Void
  -> Sem r a
voidRouter (RoutedTx tx) =
  let Tx{txMsg} = tx
      Msg{msgData} = txMsg
  in pure $ absurd msgData

--------------------------------------------------------------------------------

class Eval ms core where
  type Effs ms core :: EffectRow
  eval :: Modules ms r
       -> forall a. Sem (Effs ms core) a
       -> Sem (BaseApp core) a

instance Eval '[Module name msg api s r] core where
  type Effs '[Module name msg api s r] core = s :& BaseApp core
  eval (m :+ NilModules) = moduleEval m

instance (Members BaseAppEffs (Effs (m' ': ms) core),  Eval (m' ': ms) core) => Eval (Module name msg api s r ': m' ': ms) core where
  type Effs (Module name msg api s r ': m' ': ms) core = s :& (Effs (m': ms)) core
  eval (m :+ rest) = eval rest . moduleEval m
