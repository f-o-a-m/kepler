{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.Application.Module
  ( Module(..)
  , Component
  , ModuleEffs
  , ModuleList(..)
  , Application(..)
  , ToApplication(..)
  , hoistApplication
  , Eval(..)
  , makeApplication
  , applyAnteHandler

  ) where

import           Data.Kind                           (Type)
import           Data.Proxy
import           GHC.TypeLits                        (ErrorMessage (..), Symbol,
                                                      TypeError)
import qualified Network.ABCI.Types.Messages.Request as Req
import           Polysemy                            (EffectRow, Members, Sem)
import           Servant.API                         ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp              ((:&), BaseAppEffs,
                                                      BaseEffs)
import qualified Tendermint.SDK.BaseApp.Query        as Q
import           Tendermint.SDK.BaseApp.Store        (Scope (..))
import qualified Tendermint.SDK.BaseApp.Transaction  as T
-- import qualified Network.ABCI.Types.Messages.Response     as Resp

type Component = EffectRow -> Type

-- NOTE: This does not pull in transitive dependencies on purpose to avoid
-- unintended enlarged scope
type family DependencyEffs (ms :: [Component]) :: EffectRow where
  DependencyEffs '[] = '[]
  DependencyEffs (Module _ _ _ _ es deps ': rest) = es :& DependencyEffs rest
  DependencyEffs _ = TypeError ('Text "DependencyEffs is a partial function defined only on partially applied Modules")

data Module (name :: Symbol) (check :: Type) (deliver :: Type) (query :: Type) (es :: EffectRow) (deps :: [Component]) (r :: EffectRow) = Module
  { moduleTxChecker :: T.RouteTx check r
  , moduleTxDeliverer :: T.RouteTx deliver r
  , moduleQuerier :: Q.RouteQ query r
  , moduleBeginBlock ::  Req.BeginBlock -> Sem r ()
  , moduleEval :: forall s. (Members T.TxEffs s, Members BaseEffs s, Members (DependencyEffs deps) s) => forall a. Sem (es :& s) a -> Sem s a
  }

type family ModuleEffs (m :: Component) :: EffectRow where
  ModuleEffs (Module _ _ _ _ es deps) = es :& DependencyEffs deps :& T.TxEffs :& BaseEffs
  ModuleEffs _ = TypeError ('Text "ModuleEffs is a partial function defined only on Component")

data ModuleList (ms :: [Component]) r where
  NilModules :: ModuleList '[] r
  (:+) :: Module name check deliver query es deps r
       -> ModuleList ms r
       -> ModuleList (Module name check deliver query es deps ': ms) r

infixr 5 :+

data Application check deliver query r s = Application
  { applicationTxChecker   :: T.RouteTx check r
  , applicationTxDeliverer :: T.RouteTx deliver r
  , applicationQuerier     :: Q.RouteQ query s
  , applicationBeginBlock  :: Req.BeginBlock -> Sem r ()
  }

class ToApplication ms r where
  type ApplicationC ms :: Type
  type ApplicationD ms :: Type
  type ApplicationQ ms :: Type

  toApplication :: ModuleList ms r -> Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) r r

instance ToApplication '[Module name check deliver query es deps] r where
  type ApplicationC '[Module name check deliver query es deps] = name :> check
  type ApplicationD '[Module name check deliver query es deps] = name :> deliver
  type ApplicationQ '[Module name check deliver query es deps] = name :> query

  toApplication (Module{..} :+ NilModules) =
    Application
      { applicationTxChecker = moduleTxChecker
      , applicationTxDeliverer = moduleTxDeliverer
      , applicationQuerier = moduleQuerier
      , applicationBeginBlock = moduleBeginBlock
      }

instance ToApplication (m' ': ms) r => ToApplication (Module name check deliver query es deps ': m' ': ms) r where
  type ApplicationC (Module name check deliver query es deps ': m' ': ms) = (name :> check) :<|> ApplicationC (m' ': ms)
  type ApplicationD (Module name check deliver query es deps ': m' ': ms) = (name :> deliver) :<|> ApplicationD (m' ': ms)
  type ApplicationQ (Module name check deliver query es deps ': m' ': ms) = (name :> query) :<|> ApplicationQ (m' ': ms)

  toApplication (Module{..} :+ rest) =
    let app = toApplication rest
    in Application
         { applicationTxChecker = moduleTxChecker :<|> applicationTxChecker app
         , applicationTxDeliverer = moduleTxDeliverer :<|> applicationTxDeliverer app
         , applicationQuerier = moduleQuerier :<|> applicationQuerier app
         , applicationBeginBlock = moduleBeginBlock >> applicationBeginBlock app
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
    , applicationBeginBlock = natT . applicationBeginBlock app
    }

class Eval ms (core :: EffectRow) where
  type Effs ms core :: EffectRow
  eval
    :: proxy core
    -> ModuleList ms r
    -> forall a.
       Sem (Effs ms core) a
    -> Sem (T.TxEffs :& BaseAppEffs core) a

instance (DependencyEffs deps ~ '[]) => Eval '[Module name check deliver query es deps] core where
  type Effs '[Module name check deliver query es deps] core = es :& T.TxEffs :& BaseAppEffs core
  eval _ (m :+ NilModules) = moduleEval m

instance ( Members (DependencyEffs deps) (Effs (m' ': ms) s)
         , Members T.TxEffs (Effs (m' ': ms) s)
         , Members BaseEffs (Effs (m' ': ms) s)
         , Eval (m' ': ms) s
         ) => Eval (Module name check deliver query es deps ': m' ': ms) s where
  type Effs (Module name check deliver query es deps ': m' ': ms) s = es :& (Effs (m': ms)) s
  eval pcore (m :+ rest) = eval pcore rest . moduleEval m

makeApplication
  :: Eval ms core
  => ToApplication ms (Effs ms core)
  => T.HasTxRouter (ApplicationC ms) (Effs ms core) 'QueryAndMempool
  => T.HasTxRouter (ApplicationD ms) (Effs ms core) 'Consensus
  => Q.HasQueryRouter (ApplicationQ ms) (Effs ms core)
  => Proxy core
  -> T.AnteHandler (Effs ms core)
  -> ModuleList ms (Effs ms core)
  -> Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) (T.TxEffs :& BaseAppEffs core) (Q.QueryEffs :& BaseAppEffs core)
makeApplication p@(Proxy :: Proxy core) ah (ms :: ModuleList ms (Effs ms core)) =
  let app = applyAnteHandler ah $ toApplication ms :: Application (ApplicationC ms) (ApplicationD ms) (ApplicationQ ms) (Effs ms core) (Effs ms core)
      -- WEIRD: if you move the eval into a separate let binding then it doesn't typecheck...
  in hoistApplication (eval @ms @core p ms) (T.evalReadOnly . eval @ms @core p ms) app

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
