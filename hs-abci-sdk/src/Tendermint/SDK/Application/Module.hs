{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.Application.Module
  ( Module(..)
  , voidModuleMessages
  , defaultTxChecker
  , Modules(..)
  , QueryRouter(Api)
  , queryRouter
  , RoutingContext(..)
  , Router(..)
  , TxRouter
  , txRouter
  , voidRouter
  , Eval(..)
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Data.ByteString                    (ByteString)
import           Data.Proxy
import           Data.String.Conversions            (cs)
import qualified Data.Validation                    as V
import           Data.Void
import           GHC.TypeLits                       (KnownSymbol, Symbol,
                                                     symbolVal)
import           Polysemy                           (EffectRow, Embed, Member,
                                                     Members, Sem)
import           Polysemy.Error                     (Error)
import           Servant.API                        ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp             ((:&), AppError, BaseApp,
                                                     BaseAppEffs, SDKError (..),
                                                     throwSDKError)
import qualified Tendermint.SDK.BaseApp.Query       as Q
import qualified Tendermint.SDK.BaseApp.Transaction as T
import           Tendermint.SDK.Codec               (HasCodec (..))
import           Tendermint.SDK.Types.Message       (Msg (..),
                                                     ValidateMessage (..),
                                                     formatMessageSemanticError)
import           Tendermint.SDK.Types.Transaction   (PreRoutedTx (..), Tx (..))
import           Tendermint.SDK.Types.TxResult      (TxResult)

data Module (name :: Symbol) h q (s :: EffectRow) (r :: EffectRow) = Module
  { moduleTxDeliverer :: T.RouteTx h r 'DeliverTx
  , moduleTxChecker   :: T.RouteTx h r 'CheckTx
  , moduleQueryServer :: Q.RouteQ api r
  , moduleEval :: forall deps. Members BaseAppEffs deps => forall a. Sem (s :& deps) a -> Sem deps a
  }

data Modules (ms :: [*]) r where
    NilModules :: Modules '[] r
    (:+) :: Module name h q s r -> Modules ms r -> Modules (Module name h q s r  ': ms) r

infixr 5 :+

--------------------------------------------------------------------------------

appQueryRouter
  :: AppQueryRouter ms r
  => Q.HasRouter (Api ms) r
  => Modules ms r
  -> Q.QueryApplication (Sem r)
appQueryRouter (ms :: Modules ms r) =
  Q.serve (Proxy :: Proxy (QApi ms)) (Proxy :: Proxy r) (routeAppQuery ms)

class AppQueryRouter ms r where
    type QApi ms :: *
    routeAppQuery :: Modules ms r -> Q.RouteT (QApi ms) r

instance AppQueryRouter '[Module name h q s r] r where
    type QApi '[Module name h q s r] = name :> q
    routeAppQuery (m :+ NilModules) = moduleQueryServer m

instance AppQueryRouter (m' ': ms) r => QueryRouter (Module name h q s r ': m' ': ms) r where
    type QApi (Module name h q s r ': m' ': ms) = (name :> q) :<|> QApi (m' ': ms)
    routeAppQuery (m :+ rest) = moduleQueryServer m :<|> routeAppQuery rest

--------------------------------------------------------------------------------

txRouter
  :: AppTxRouter ms r
  => T.HasTxRouter (Api ms) r
  => Modules ms r
  -> Proxy ( c :: RouteContext)
  -> Q.QueryApplication (Sem r)
txRouter (ms :: Modules ms r) = Q.serve (Proxy :: Proxy (Api ms)) (Proxy :: Proxy r) (routeQuery ms)

class AppTxRouter ms r c where
    type Api ms :: *
    routeAppTx :: Modules ms r -> T.RouteTx (Api ms) r c

instance QueryRouter '[Module name msg val api s r] r where
    type Api '[Module name msg val api s r] = name :> api
    routeQuery (m :+ NilModules) = moduleQueryServer m

instance QueryRouter (m' ': ms) r => QueryRouter (Module name msg val api s r ': m' ': ms) r where
    type Api (Module name msg val api s r ': m' ': ms) = (name :> api) :<|> Api (m' ': ms)
    routeQuery (m :+ rest) = moduleQueryServer m :<|> routeQuery rest

--------------------------------------------------------------------------------

class Eval ms core where
  type Effs ms core :: EffectRow
  eval :: Modules ms r
       -> forall a. Sem (Effs ms core) a
       -> Sem (BaseApp core) a

instance Eval '[Module name msg val api s r] core where
  type Effs '[Module name msg val api s r] core = s :& BaseApp core
  eval (m :+ NilModules) = moduleEval m

instance (Members BaseAppEffs (Effs (m' ': ms) core),  Eval (m' ': ms) core) => Eval (Module name msg val api s r ': m' ': ms) core where
  type Effs (Module name msg val api s r ': m' ': ms) core = s :& (Effs (m': ms)) core
  eval (m :+ rest) = eval rest . moduleEval m
