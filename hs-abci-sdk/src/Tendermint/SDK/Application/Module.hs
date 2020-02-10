{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.Application.Module where

import           Data.Proxy
import           GHC.TypeLits                       (Symbol)
import           Polysemy                           (EffectRow, Members, Sem)
import           Servant.API                        ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp             ((:&), BaseAppEffs)
import qualified Tendermint.SDK.BaseApp.Query       as Q
import qualified Tendermint.SDK.BaseApp.Transaction as T

data Module (name :: Symbol) (check :: *) (deliver :: *) (query :: *) (es :: EffectRow) (r :: EffectRow)  = Module
  { moduleTxChecker :: T.RouteTx check r
  , moduleTxDeliverer :: T.RouteTx deliver r
  , moduleQuerier :: Q.RouteQ query r
  , moduleEval :: forall deps. (Members BaseAppEffs deps, Members T.TxEffs deps) => forall a. Sem (es :& deps) a -> Sem deps a
  }

data ModuleList ms r where
  NilModules :: ModuleList '[] r
  (:+) :: Module name check deliver query es r -> ModuleList ms r -> ModuleList (Module name check deliver query es r ': ms) r

infixr 5 :+

data Application check deliver query r s = Application
  { applicationTxChecker   :: T.RouteTx check r
  , applicationTxDeliverer :: T.RouteTx deliver r
  , applicationQuerier     :: Q.RouteQ query s
  }

class ToApplication ms r where
  type ApplicationC ms :: *
  type ApplicationD ms :: *
  type ApplicationQ ms :: *

  toApplication :: ModuleList ms r -> Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) r r

instance ToApplication '[Module name check deliver query es r] r where
  type ApplicationC '[Module name check deliver query es r] = name :> check
  type ApplicationD '[Module name check deliver query es r] = name :> deliver
  type ApplicationQ '[Module name check deliver query es r] = name :> query

  toApplication (Module{..} :+ NilModules) =
    Application
      { applicationTxChecker = moduleTxChecker
      , applicationTxDeliverer = moduleTxDeliverer
      , applicationQuerier = moduleQuerier
      }

instance ToApplication (m' ': ms) r => ToApplication (Module name check deliver query es r ': m' ': ms) r where
  type ApplicationC (Module name check deliver query es r ': m' ': ms) = (name :> check) :<|> ApplicationC (m' ': ms)
  type ApplicationD (Module name check deliver query es r ': m' ': ms) = (name :> deliver) :<|> ApplicationD (m' ': ms)
  type ApplicationQ (Module name check deliver query es r ': m' ': ms) = (name :> query) :<|> ApplicationQ (m' ': ms)

  toApplication (Module{..} :+ rest) =
    let app = toApplication rest
    in Application
         { applicationTxChecker = moduleTxChecker :<|> applicationTxChecker app
         , applicationTxDeliverer = moduleTxDeliverer :<|> applicationTxDeliverer app
         , applicationQuerier = moduleQuerier :<|> applicationQuerier app
         }

hoistApplication
  :: T.HasTxRouter check r
  => T.HasTxRouter deliver r
  => Q.HasQueryRouter query s
  => (forall a. Sem r a -> Sem r' a)
  -> (forall a. Sem s a -> Sem s' a)
  -> Application check deliver query r s
  -> Application check deliver query r' s'
hoistApplication natT natQ (app :: Application check deliver query r s) =
  Application
    { applicationTxChecker = T.hoistTxRouter (Proxy @check) (Proxy @r) natT $ applicationTxChecker app
    , applicationTxDeliverer = T.hoistTxRouter (Proxy @deliver) (Proxy @r) natT $ applicationTxDeliverer app
    , applicationQuerier = Q.hoistQueryRouter (Proxy @query) (Proxy @s) natQ $ applicationQuerier app
    }

class Eval ms core where
  type Effs ms core :: EffectRow
  eval :: ModuleList ms r
       -> (forall a. Sem (Effs ms core) a -> Sem (T.TxEffs :& BaseAppEffs :& core) a)

instance Eval '[Module name check deliver query es r] core where
  type Effs '[Module name check deliver query es r] core = es :& T.TxEffs :& BaseAppEffs :& core
  eval (m :+ NilModules) = moduleEval m

instance ( Members (T.TxEffs :& BaseAppEffs) (Effs (m' ': ms) core)
         , Eval (m' ': ms) core
         ) => Eval (Module name check deliver query es r ': m' ': ms) core where
  type Effs (Module name check deliver query es r ': m' ': ms) core = es :& (Effs (m': ms)) core
  eval (m :+ rest) = eval rest . moduleEval m

makeApplication
  :: Eval ms core
  => ToApplication ms (Effs ms core)
  => T.HasTxRouter (ApplicationC ms) (Effs ms core)
  => T.HasTxRouter (ApplicationD ms) (Effs ms core)
  => Q.HasQueryRouter (ApplicationQ ms) (Effs ms core)
  => Proxy core
  -> ModuleList ms (Effs ms core)
  -> Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) (T.TxEffs :& BaseAppEffs :& core) (Q.QueryEffs :& BaseAppEffs :& core)
makeApplication (Proxy :: Proxy core) (ms :: ModuleList ms (Effs ms core)) =
  let app = toApplication ms :: Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) (Effs ms core) (Effs ms core)
      -- WEIRD: if you move the eval into a separate let binding then it doesn't typecheck...
  in hoistApplication (eval @ms @core ms) (T.evalReadOnly . eval @ms @core ms) app


