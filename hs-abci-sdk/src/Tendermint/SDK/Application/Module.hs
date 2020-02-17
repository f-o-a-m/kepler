{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.Application.Module
  ( Module(..)
  , ModuleMembers
  , ModuleList(..)
  , Application(..)
  , ToApplication(..)
  , hoistApplication
  , Eval(..)
  , makeApplication
  , applyAnteHandler

  ) where

import           Data.Kind                          (Constraint)
import           Data.Proxy
import           GHC.TypeLits                       (Symbol)
import           Polysemy                           (EffectRow, Members, Sem)
import           Servant.API                        ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp             ((:&), BaseEffs)
import qualified Tendermint.SDK.BaseApp.Query       as Q
import           Tendermint.SDK.BaseApp.Store       (Scope (..))
import qualified Tendermint.SDK.BaseApp.Transaction as T

-- NOTE: This does not pull in transitive dependencies on purpose to avoid
-- unintended enlarged scope
type family DependencyEffs (ms :: [EffectRow -> *]) :: EffectRow where
  DependencyEffs '[] = '[]
  DependencyEffs (Module _ _ _ _ es deps ': rest) = es :& DependencyEffs rest

data Module (name :: Symbol) (check :: *) (deliver :: *) (query :: *) (es :: EffectRow) (deps :: [EffectRow -> *]) (r :: EffectRow) = Module
  { moduleTxChecker :: T.RouteTx check r
  , moduleTxDeliverer :: T.RouteTx deliver r
  , moduleQuerier :: Q.RouteQ query r
  , moduleEval :: forall s. (Members T.TxEffs s, Members (DependencyEffs deps) s) => forall a. Sem (es :& s) a -> Sem s a
  }

type family ModuleMembers (m :: EffectRow -> *) (r :: EffectRow) :: Constraint where
  ModuleMembers (Module _ _ _ _ es deps) r =
    (Members es r, Members (DependencyEffs deps) r, Members T.TxEffs r, Members BaseEffs r)

data ModuleList ms r where
  NilModules :: ModuleList '[] r
  (:+) :: Module name check deliver query es deps r
       -> ModuleList ms r
       -> ModuleList (Module name check deliver query es deps r ': ms) r

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

instance ToApplication '[Module name check deliver query es deps r] r where
  type ApplicationC '[Module name check deliver query es deps r] = name :> check
  type ApplicationD '[Module name check deliver query es deps r] = name :> deliver
  type ApplicationQ '[Module name check deliver query es deps r] = name :> query

  toApplication (Module{..} :+ NilModules) =
    Application
      { applicationTxChecker = moduleTxChecker
      , applicationTxDeliverer = moduleTxDeliverer
      , applicationQuerier = moduleQuerier
      }

instance ToApplication (m' ': ms) r => ToApplication (Module name check deliver query es deps r ': m' ': ms) r where
  type ApplicationC (Module name check deliver query es deps r ': m' ': ms) = (name :> check) :<|> ApplicationC (m' ': ms)
  type ApplicationD (Module name check deliver query es deps r ': m' ': ms) = (name :> deliver) :<|> ApplicationD (m' ': ms)
  type ApplicationQ (Module name check deliver query es deps r ': m' ': ms) = (name :> query) :<|> ApplicationQ (m' ': ms)

  toApplication (Module{..} :+ rest) =
    let app = toApplication rest
    in Application
         { applicationTxChecker = moduleTxChecker :<|> applicationTxChecker app
         , applicationTxDeliverer = moduleTxDeliverer :<|> applicationTxDeliverer app
         , applicationQuerier = moduleQuerier :<|> applicationQuerier app
         }

hoistApplication
  :: T.HasTxRouter check r 'QueryAndMempool
  => T.HasTxRouter deliver r 'Consensus
  => Q.HasQueryRouter query s
  => (forall a. Sem r a -> Sem r' a)
  -> (forall a. Sem s a -> Sem s' a)
  -> Application check deliver query r s
  -> Application check deliver query r' s'
hoistApplication natT natQ (app :: Application check deliver query r s) =
  Application
    { applicationTxChecker = T.hoistTxRouter (Proxy @check) (Proxy @r) (Proxy @'QueryAndMempool) natT $ applicationTxChecker app
    , applicationTxDeliverer = T.hoistTxRouter (Proxy @deliver) (Proxy @r) (Proxy @'Consensus) natT $ applicationTxDeliverer app
    , applicationQuerier = Q.hoistQueryRouter (Proxy @query) (Proxy @s) natQ $ applicationQuerier app
    }

class Eval ms s where
  type Effs ms s :: EffectRow
  eval
    :: ModuleList ms r
    -> forall a.
       Sem (Effs ms s) a
    -> Sem (T.TxEffs :& s) a

instance (DependencyEffs deps ~ '[]) => Eval '[Module name check deliver query es deps r] s where
  type Effs '[Module name check deliver query es deps r] s = es :& T.TxEffs :& s
  eval (m :+ NilModules) = moduleEval m

instance ( Members (DependencyEffs deps) (Effs (m' ': ms) s)
         , Members T.TxEffs (Effs (m' ': ms) s)
         , Eval (m' ': ms) s
         ) => Eval (Module name check deliver query es deps r ': m' ': ms) s where
  type Effs (Module name check deliver query es deps r ': m' ': ms) s = es :& (Effs (m': ms)) s
  eval (m :+ rest) = eval rest . moduleEval m

makeApplication
  :: Eval ms deps
  => ToApplication ms (Effs ms deps)
  => T.HasTxRouter (ApplicationC ms) (Effs ms deps) 'QueryAndMempool
  => T.HasTxRouter (ApplicationD ms) (Effs ms deps) 'Consensus
  => Q.HasQueryRouter (ApplicationQ ms) (Effs ms deps)
  => Proxy deps
  -> T.AnteHandler (Effs ms deps)
  -> ModuleList ms (Effs ms deps)
  -> Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) (T.TxEffs :& deps) (Q.QueryEffs :& deps)
makeApplication (Proxy :: Proxy deps) ah (ms :: ModuleList ms (Effs ms deps)) =
  let app = applyAnteHandler ah $ toApplication ms :: Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) (Effs ms deps) (Effs ms deps)
      -- WEIRD: if you move the eval into a separate let binding then it doesn't typecheck...
  in hoistApplication (eval @ms @deps ms) (T.evalReadOnly . eval @ms @deps ms) app

applyAnteHandler
  :: T.HasTxRouter check r 'QueryAndMempool
  => T.HasTxRouter deliver r 'Consensus
  => T.AnteHandler r
  -> Application check deliver query r s
  -> Application check deliver query r s
applyAnteHandler ah (app ::  Application check deliver query r s) =
  app { applicationTxChecker = T.applyAnteHandler (Proxy @check) (Proxy @r) (Proxy @'QueryAndMempool) ah $
          applicationTxChecker app
      , applicationTxDeliverer = T.applyAnteHandler (Proxy @deliver) (Proxy @r) (Proxy @'Consensus) ah $
          applicationTxDeliverer app
      }
