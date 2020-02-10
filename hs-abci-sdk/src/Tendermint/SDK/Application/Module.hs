{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.Application.Module where
 -- ( Module(..)
 -- , Modules(..)
 -- , AppQueryRouter(..)
 -- , appQueryRouter
 -- , AppTxRouter(..)
 -- , appTxRouter
 -- , Eval(..)
 -- ) where

import           Data.Proxy
import           Data.Singletons                    (Sing)
import           GHC.TypeLits                       (Symbol)
import           Polysemy                           (EffectRow, Member, Members,
                                                     Sem)
import           Servant.API                        ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp             ((:&), BaseAppEffs,
                                                     WriteStore)
import qualified Tendermint.SDK.BaseApp.Query       as Q
import qualified Tendermint.SDK.BaseApp.Transaction as T
import Tendermint.SDK.BaseApp.Store (ReadStore, WriteStore)

data Module (name :: Symbol) (t :: *) (es :: EffectRow) (r :: EffectRow)  = Module
  { moduleTxChecker :: T.RouteTx t r 'T.CheckTx
  , moduleTxDeliverer :: T.RouteTx t r 'T.DeliverTx
  , moduleTxEval :: forall deps. (Members [WriteStore, ReadStore] deps, Members BaseAppEffs deps, Members T.TxEffs deps) => forall a. Sem (es :& deps) a -> Sem deps a
  }

data ModuleList ms r where
  NilModules :: ModuleList '[] r
  (:+) :: Module name t es r -> ModuleList ms r -> ModuleList (Module name t es r ': ms) r

infixr 5 :+

data TxApplication t r = TxApplication
  { txApplicationTxChecker :: T.RouteTx t r 'T.CheckTx
  , txApplicationTxDeliverer :: T.RouteTx t r 'T.DeliverTx
  }

class ToTxApplication ms r where
  type TxApplicationT ms :: *
  
  toTxApplication :: ModuleList ms r -> TxApplication (TxApplicationT ms) r

instance ToTxApplication '[Module name t es r] r where
  type TxApplicationT '[Module name t es r] = name :> t

  toTxApplication (Module{..} :+ NilModules) = 
    TxApplication
      { txApplicationTxChecker = moduleTxChecker
      , txApplicationTxDeliverer = moduleTxDeliverer
      }

instance ToTxApplication (m' ': ms) r => ToTxApplication (Module name t es r ': m' ': ms) r where
  type TxApplicationT (Module name t es r ': m' ': ms) = (name :> t) :<|> TxApplicationT (m' ': ms)

  toTxApplication (Module{..} :+ rest) = 
    let app = toTxApplication rest
    in TxApplication
         { txApplicationTxChecker = moduleTxChecker :<|> txApplicationTxChecker app
         , txApplicationTxDeliverer = moduleTxDeliverer :<|> txApplicationTxDeliverer app
         }


hoistTxApplication 
  :: T.HasTxRouter t r 'T.CheckTx
  => T.HasTxRouter t r 'T.DeliverTx
  => (forall a. Sem r a -> Sem r' a)
  -> TxApplication t r
  -> TxApplication t r'
hoistTxApplication natT (app :: TxApplication t r) = 
  TxApplication
    { txApplicationTxChecker = T.hoistTxRouter (Proxy @t) (Proxy @r) (Proxy @'T.CheckTx) natT $ txApplicationTxChecker app
    , txApplicationTxDeliverer = T.hoistTxRouter (Proxy @t) (Proxy @r) (Proxy @'T.DeliverTx) natT $ txApplicationTxDeliverer app
    }

class Eval ms core where
  type Effs ms core :: EffectRow
  eval :: ModuleList ms r
       -> (forall a. Sem (Effs ms core) a -> Sem (T.TxEffs :& BaseAppEffs :& core) a)

instance Eval '[Module name t es r] core where
  type Effs '[Module name t es r] core = es :& T.TxEffs :& BaseAppEffs :& core
  eval (m :+ NilModules) = moduleTxEval m

instance (Members (T.TxEffs :& BaseAppEffs) (Effs (m' ': ms) core),  Eval (m' ': ms) core) => Eval (Module name t es r ': m' ': ms) core where
  type Effs (Module name t es r ': m' ': ms) core = es :& (Effs (m': ms)) core
  eval (m :+ rest) = eval rest . moduleTxEval m

makeTxApplication
  :: Eval ms core
  => ToTxApplication ms (Effs ms core)
  => T.HasTxRouter (TxApplicationT ms) (Effs ms core) 'T.CheckTx
  => T.HasTxRouter (TxApplicationT ms) (Effs ms core) 'T.DeliverTx
  => Proxy core
  -> ModuleList ms (Effs ms core)
  -> T.RouteTx (TxApplicationT ms) (T.TxEffs :& BaseAppEffs :& core) 'T.DeliverTx
makeTxApplication (Proxy :: Proxy core) (ms :: ModuleList ms (Effs ms core)) =
  let app = toTxApplication ms :: TxApplication (TxApplicationT ms) (Effs ms core)
      -- WEIRD: if you move the eval into a separate let binding then it doesn't typecheck...
      app' = hoistTxApplication (eval @ms @core ms) app
  in txApplicationTxDeliverer app'
  


-- --------------------------------------------------------------------------------
-- 
-- appQueryRouter
--   :: AppQueryRouter ms r
--   => Q.HasQueryRouter (QApi ms) r
--   => Modules ms r
--   -> Q.QueryApplication (Sem r)
-- appQueryRouter (ms :: Modules ms r) =
--   Q.serveQueryApplication (Proxy :: Proxy (QApi ms)) (Proxy :: Proxy r) (routeAppQuery ms)
-- 
-- class AppQueryRouter ms r where
--     type QApi ms :: *
--     routeAppQuery :: Modules ms r -> Q.RouteQ (QApi ms) r
-- 
-- instance AppQueryRouter '[Module name h q s r] r where
--     type QApi '[Module name h q s r] = name :> q
--     routeAppQuery (m :+ NilModules) = moduleQueryServer m
-- 
-- instance AppQueryRouter (m' ': ms) r => AppQueryRouter (Module name h q s r ': m' ': ms) r where
--     type QApi (Module name h q s r ': m' ': ms) = (name :> q) :<|> QApi (m' ': ms)
--     routeAppQuery (m :+ rest) = moduleQueryServer m :<|> routeAppQuery rest
-- 
-- --------------------------------------------------------------------------------
-- 
-- appTxRouter
--   :: AppTxRouter ms r 'T.DeliverTx
--   => AppTxRouter ms r 'T.CheckTx
--   => T.HasTxRouter (TApi ms) (WriteStore ': r) 'T.DeliverTx
--   => T.HasTxRouter (TApi ms) r 'T.CheckTx
--   => Modules ms r
--   -> Sing (c :: T.RouteContext)
--   -> T.TransactionApplication (Sem (AddWriteStore c r))
-- appTxRouter (ms :: Modules ms r) ctx =
--   case ctx of
--     T.SCheckTx ->
--       T.serveTxApplication (Proxy :: Proxy (TApi ms)) (Proxy :: Proxy r)
--            ctx (routeAppTx ctx ms)
--     T.SDeliverTx ->
--       T.serveTxApplication (Proxy :: Proxy (TApi ms)) (Proxy :: Proxy (WriteStore ': r))
--            ctx (routeAppTx ctx ms)
-- 
-- type family AddWriteStore (c :: T.RouteContext) (r :: EffectRow) :: EffectRow where
--   AddWriteStore 'T.CheckTx r = r
--   AddWriteStore 'T.DeliverTx r = (WriteStore ': r)
-- 
-- class AppTxRouter ms r (c :: T.RouteContext) where
--     type TApi ms :: *
--     routeAppTx :: Sing c -> Modules ms r -> T.RouteTx (TApi ms) (AddWriteStore c r) c
-- 
-- instance AppTxRouter '[Module name h q s r] r 'T.CheckTx where
--     type TApi '[Module name h q s r] = name :> h
--     routeAppTx _ (m :+ NilModules) = moduleTxChecker m
-- 
-- instance AppTxRouter (m' ': ms) r 'T.CheckTx => AppTxRouter (Module name h q s r ': m' ': ms) r 'T.CheckTx where
--     type TApi (Module name h q s r ': m' ': ms) = (name :> h) :<|> TApi (m' ': ms)
--     routeAppTx pc (m :+ rest) = moduleTxChecker m :<|> routeAppTx pc rest
-- 
-- instance AppTxRouter '[Module name h q s r] r 'T.DeliverTx where
--     type TApi '[Module name h q s r] = name :> h
--     routeAppTx _ (m :+ NilModules) = moduleTxDeliverer m
-- 
-- instance AppTxRouter (m' ': ms) r 'T.DeliverTx => AppTxRouter (Module name h q s r ': m' ': ms) r 'T.DeliverTx where
--     type TApi (Module name h q s r ': m' ': ms) = (name :> h) :<|> TApi (m' ': ms)
--     routeAppTx pc (m :+ rest) = moduleTxDeliverer m :<|> routeAppTx pc rest
-- 
-- --------------------------------------------------------------------------------
-- 
-- -}