{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.BaseApp.Block.Router where

import           Data.Kind                                (Type)
import           Data.Proxy
import           Network.ABCI.Types.Messages.Response     as Response (BeginBlock,
                                                                       EndBlock (EndBlock))
import           Polysemy                                 (EffectRow, Embed,
                                                           Members, Sem)
import           Polysemy.Tagged                          (Tagged)
import           Servant.API
import           Tendermint.SDK.BaseApp.Block.Effect
import           Tendermint.SDK.BaseApp.Block.Types       (BeginBlockRequest,
                                                           EmptyBeginBlockServer,
                                                           EmptyEndBlockServer,
                                                           EndBlockRequest)
import qualified Tendermint.SDK.BaseApp.Router            as R
import           Tendermint.SDK.BaseApp.Store
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Types.Effects             ((:&))

---------------------------------------- BeginBlock ----------------------------------------

class HasBeginBlockRouter layout (r :: EffectRow) where
  type RouteBB layout r :: Type
  routeBB ::
    Proxy layout ->
    Proxy r ->
    R.Delayed (Sem r) env BeginBlockRequest (RouteBB layout (BlockEffs :& r)) ->
    R.Router env r BeginBlockRequest Response.BeginBlock

  hoistBeginBlockRouter :: Proxy layout -> Proxy r -> (forall a. Sem s a -> Sem s' a) -> RouteBB layout s -> RouteBB layout s'

instance (HasBeginBlockRouter a r, HasBeginBlockRouter b r) => HasBeginBlockRouter (a :<|> b) r where
  -- replace with merged choice
  -- always keeps the first beginBlock response
  type RouteBB (a :<|> b) r = RouteBB a r :<|> RouteBB b r
  routeBB _ pr server =
    R.ChoiceMerge
      (routeBB (Proxy @a) pr ((\(a :<|> _) -> a) <$> server))
      (routeBB (Proxy @b) pr ((\(_ :<|> b) -> b) <$> server))
      (R.makeMerge const)

  hoistBeginBlockRouter _ pr nat (a :<|> b) =
    hoistBeginBlockRouter (Proxy @a) pr nat a :<|> hoistBeginBlockRouter (Proxy @b) pr nat b

instance HasBeginBlockRouter sublayout r => HasBeginBlockRouter (path :> sublayout) r where
  -- replace with inner router without name
  type RouteBB (path :> sublayout) r = RouteBB sublayout r
  routeBB _ pr subserver = routeBB (Proxy :: Proxy sublayout) pr subserver

  hoistBeginBlockRouter _ pr nat = hoistBeginBlockRouter (Proxy @sublayout) pr nat

instance
  (Members[Tagged 'Consensus WriteStore, Tagged 'Consensus ReadStore, Embed IO] r) =>
  HasBeginBlockRouter (BeginBlockRequest :~> Return Response.BeginBlock) r
  where
  type RouteBB (BeginBlockRequest :~> Return Response.BeginBlock) r = BeginBlockRequest -> Sem r Response.BeginBlock

  routeBB _ _ server = R.leafRouter (\env b -> R.runAction (runBeginBlock <$> R.addBody server (R.withRequest pure)) env b (pure . R.Route))
  hoistBeginBlockRouter _ _ nat = (.) nat

instance HasBeginBlockRouter EmptyBeginBlockServer r where
  type RouteBB EmptyBeginBlockServer r = EmptyBeginBlockServer
  routeBB _ _ _ = R.StaticRouter mempty mempty
  hoistBeginBlockRouter _ _ _ = id

---------------------------------------- EndBlock ----------------------------------------

class HasEndBlockRouter layout (r :: EffectRow) where
  type RouteEB layout r :: Type
  routeEB ::
    Proxy layout ->
    Proxy r ->
    R.Delayed (Sem r) env EndBlockRequest (RouteEB layout (BlockEffs :& r)) ->
    R.Router env r EndBlockRequest Response.EndBlock

  hoistEndBlockRouter :: Proxy layout -> Proxy r -> (forall a. Sem s a -> Sem s' a) -> RouteEB layout s -> RouteEB layout s'

instance (HasEndBlockRouter a r, HasEndBlockRouter b r) => HasEndBlockRouter (a :<|> b) r where
  -- replace with merged choice
  type RouteEB (a :<|> b) r = RouteEB a r :<|> RouteEB b r
  routeEB _ pr server =
    R.ChoiceMerge
      (routeEB (Proxy @a) pr ((\(a :<|> _) -> a) <$> server))
      (routeEB (Proxy @b) pr ((\(_ :<|> b) -> b) <$> server))
      (R.makeMerge endBlockMerge)

  hoistEndBlockRouter _ pr nat (a :<|> b) =
    hoistEndBlockRouter (Proxy @a) pr nat a :<|> hoistEndBlockRouter (Proxy @b) pr nat b

instance HasEndBlockRouter sublayout r => HasEndBlockRouter (path :> sublayout) r where
  -- replace with inner router without name
  type RouteEB (path :> sublayout) r = RouteEB sublayout r
  routeEB _ pr subserver = routeEB (Proxy :: Proxy sublayout) pr subserver

  hoistEndBlockRouter _ pr nat = hoistEndBlockRouter (Proxy @sublayout) pr nat


instance
  (Members[Tagged 'Consensus WriteStore, Tagged 'Consensus ReadStore, Embed IO] r) =>
  HasEndBlockRouter (EndBlockRequest :~> Return Response.EndBlock) r
  where
  type RouteEB (EndBlockRequest :~> Return Response.EndBlock) r = EndBlockRequest -> Sem r Response.EndBlock

  routeEB _ _ server = R.leafRouter (\env b -> R.runAction (runEndBlock <$> R.addBody server (R.withRequest pure)) env b (pure . R.Route))
  hoistEndBlockRouter _ _ nat = (.) nat

-- concatenates validator updates and events
-- takes the first non-Nothing ConsensusParams update
endBlockMerge ::
  Response.EndBlock ->
  Response.EndBlock ->
  Response.EndBlock
endBlockMerge a b =
  case (a, b) of
    ( Response.EndBlock updatesA paramsA eventsA,
      Response.EndBlock updatesB paramsB eventsB
      ) -> Response.EndBlock (updatesA ++ updatesB) (mergeParams paramsA paramsB) (eventsA ++ eventsB)
  where
    mergeParams Nothing y = y
    mergeParams x _       = x

instance HasEndBlockRouter EmptyEndBlockServer r where
  type RouteEB EmptyEndBlockServer r = EmptyEndBlockServer
  routeEB _ _ _ = R.StaticRouter mempty mempty
  hoistEndBlockRouter _ _ _ = id
