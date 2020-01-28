{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.Application.Module
  ( Module(..)
  , Modules(..)
  , AppQueryRouter(..)
  , appQueryRouter
  , AppTxRouter(..)
  , appTxRouter
  , Eval(..)
  ) where

import           Data.Proxy
import           GHC.TypeLits                       (Symbol)
import           Polysemy                           (EffectRow, Members, Sem)
import           Servant.API                        ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp             ((:&), BaseApp, BaseAppEffs)
import qualified Tendermint.SDK.BaseApp.Query       as Q
import qualified Tendermint.SDK.BaseApp.Transaction as T

data Module (name :: Symbol) (h :: *) (q :: *) (s :: EffectRow) (r :: EffectRow) = Module
  { moduleTxDeliverer :: T.RouteTx h r 'T.DeliverTx
  , moduleTxChecker :: T.RouteTx h r 'T.CheckTx
  , moduleQueryServer :: Q.RouteQ q r
  , moduleEval :: forall deps. Members BaseAppEffs deps => forall a. Sem (s :& deps) a -> Sem deps a
  }

data Modules (ms :: [*]) r where
    NilModules :: Modules '[] r
    (:+) :: Module name h q s r -> Modules ms r -> Modules (Module name h q s r  ': ms) r

infixr 5 :+

--------------------------------------------------------------------------------

appQueryRouter
  :: AppQueryRouter ms r
  => Q.HasQueryRouter (QApi ms) r
  => Modules ms r
  -> Q.QueryApplication (Sem r)
appQueryRouter (ms :: Modules ms r) =
  Q.serveQueryApplication (Proxy :: Proxy (QApi ms)) (Proxy :: Proxy r) (routeAppQuery ms)

class AppQueryRouter ms r where
    type QApi ms :: *
    routeAppQuery :: Modules ms r -> Q.RouteQ (QApi ms) r

instance AppQueryRouter '[Module name h q s r] r where
    type QApi '[Module name h q s r] = name :> q
    routeAppQuery (m :+ NilModules) = moduleQueryServer m

instance AppQueryRouter (m' ': ms) r => AppQueryRouter (Module name h q s r ': m' ': ms) r where
    type QApi (Module name h q s r ': m' ': ms) = (name :> q) :<|> QApi (m' ': ms)
    routeAppQuery (m :+ rest) = moduleQueryServer m :<|> routeAppQuery rest

--------------------------------------------------------------------------------

appTxRouter
  :: AppTxRouter ms r 'T.DeliverTx
  => AppTxRouter ms r 'T.CheckTx
  => T.HasTxRouter (TApi ms) r 'T.DeliverTx
  => T.HasTxRouter (TApi ms) r 'T.CheckTx
  => Modules ms r
  -> T.RouteContext
  -> T.TransactionApplication (Sem r)
appTxRouter (ms :: Modules ms r) ctx =
  case ctx of
    T.CheckTx ->
      let checkTxP = Proxy :: Proxy 'T.CheckTx
      in T.serveTxApplication (Proxy :: Proxy (TApi ms)) (Proxy :: Proxy r)
           checkTxP (routeAppTx checkTxP ms)
    T.DeliverTx ->
      let deliverTxP = Proxy :: Proxy 'T.DeliverTx
      in T.serveTxApplication (Proxy :: Proxy (TApi ms)) (Proxy :: Proxy r)
           deliverTxP (routeAppTx deliverTxP ms)

class AppTxRouter ms r (c :: T.RouteContext) where
    type TApi ms :: *
    routeAppTx :: Proxy c -> Modules ms r -> T.RouteTx (TApi ms) r c

instance AppTxRouter '[Module name h q s r] r 'T.CheckTx where
    type TApi '[Module name h q s r] = name :> h
    routeAppTx _ (m :+ NilModules) = moduleTxChecker m

instance AppTxRouter (m' ': ms) r 'T.CheckTx => AppTxRouter (Module name h q s r ': m' ': ms) r 'T.CheckTx where
    type TApi (Module name h q s r ': m' ': ms) = (name :> h) :<|> TApi (m' ': ms)
    routeAppTx pc (m :+ rest) = moduleTxChecker m :<|> routeAppTx pc rest

instance AppTxRouter '[Module name h q s r] r 'T.DeliverTx where
    type TApi '[Module name h q s r] = name :> h
    routeAppTx _ (m :+ NilModules) = moduleTxDeliverer m

instance AppTxRouter (m' ': ms) r 'T.DeliverTx => AppTxRouter (Module name h q s r ': m' ': ms) r 'T.DeliverTx where
    type TApi (Module name h q s r ': m' ': ms) = (name :> h) :<|> TApi (m' ': ms)
    routeAppTx pc (m :+ rest) = moduleTxDeliverer m :<|> routeAppTx pc rest

--------------------------------------------------------------------------------

class Eval ms core where
  type Effs ms core :: EffectRow
  eval :: Modules ms r
       -> forall a. Sem (Effs ms core) a
       -> Sem (BaseApp core) a

instance Eval '[Module name h q s r] core where
  type Effs '[Module name h q s r] core = s :& BaseApp core
  eval (m :+ NilModules) = moduleEval m

instance (Members BaseAppEffs (Effs (m' ': ms) core),  Eval (m' ': ms) core) => Eval (Module name h q s r ': m' ': ms) core where
  type Effs (Module name h q s r ': m' ': ms) core = s :& (Effs (m': ms)) core
  eval (m :+ rest) = eval rest . moduleEval m
