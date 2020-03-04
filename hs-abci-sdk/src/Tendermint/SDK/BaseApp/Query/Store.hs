{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.BaseApp.Query.Store
  ( StoreLeaf
  , storeQueryHandler
  --, StoreQueryHandlers(..)
  ) where

--import           Control.Lens                        (to, (^.))
import           Data.ByteArray.Base64String         (fromBytes)
import           Data.Proxy
--import           Data.String.Conversions             (cs)
import           Data.Word                           (Word64)
--import           GHC.TypeLits                        (KnownSymbol, symbolVal)
import           Polysemy                            (Member, Members, Sem)
import           Polysemy.Error                      (throw)
import           Polysemy.Tagged                     (Tagged)
import           Servant.API                         ((:>))
import           Tendermint.SDK.BaseApp.Errors       (makeAppError)
import           Tendermint.SDK.BaseApp.Query.Effect (QueryEffs)
import           Tendermint.SDK.BaseApp.Query.Router (HasQueryRouter (..))
import           Tendermint.SDK.BaseApp.Query.Types  (FromQueryData, Leaf, QA,
                                                      QueryArgs (..),
                                                      QueryResult (..))
import           Tendermint.SDK.BaseApp.Router       (RouterError (..))
import           Tendermint.SDK.BaseApp.Store        (RawKey (..), ReadStore,
                                                      Scope (..), makeKeyBytes)
import qualified Tendermint.SDK.BaseApp.Store.Array  as A
import qualified Tendermint.SDK.BaseApp.Store.List   as L
import qualified Tendermint.SDK.BaseApp.Store.Map    as M
import qualified Tendermint.SDK.BaseApp.Store.Var    as V
import           Tendermint.SDK.Codec                (HasCodec)

{-

"account" :> StoreLeaf (Map Address Account) :<|>

  "count" :> StoreLeaf (Var Count) :<|>

  "counts" :> StoreLeaf (Array Count)

-}


data StoreLeaf a

instance (FromQueryData k, HasCodec v, Member (Tagged 'QueryAndMempool ReadStore) r) => HasQueryRouter (StoreLeaf (M.Map k v)) r where

   type RouteQ (StoreLeaf (M.Map k v)) r = RouteQ (QA k :> Leaf v) r
   routeQ _ = routeQ (Proxy @(QA k :> Leaf v))
   hoistQueryRouter _ pr nat f = hoistQueryRouter (Proxy @(QA k :> Leaf v)) pr nat f

instance (HasCodec a, Member (Tagged 'QueryAndMempool ReadStore) r) => HasQueryRouter (StoreLeaf (V.Var a)) r where

   type RouteQ (StoreLeaf (V.Var a)) r = RouteQ (QA () :> Leaf a) r
   routeQ _ = routeQ (Proxy @(QA () :> Leaf a))
   hoistQueryRouter _ pr nat f = hoistQueryRouter (Proxy @(QA () :> Leaf a)) pr nat f

instance (HasCodec a, Member (Tagged 'QueryAndMempool ReadStore) r) => HasQueryRouter (StoreLeaf (A.Array a)) r where

   type RouteQ (StoreLeaf (A.Array a)) r = RouteQ (QA Word64 :> Leaf a) r
   routeQ _ = routeQ (Proxy @(QA Word64 :> Leaf a))
   hoistQueryRouter _ pr nat f = hoistQueryRouter (Proxy @(QA Word64 :> Leaf a)) pr nat f

class StoreQueryHandler ns h where
    storeQueryHandler :: ns -> h

instance
  ( RawKey k
  , HasCodec v
  , Members QueryEffs r
  )
   => StoreQueryHandler (M.Map k v) (QueryArgs k -> Sem r (QueryResult v)) where
  storeQueryHandler m QueryArgs{..} = do
    let key = queryArgsData
    mRes <- M.lookup key m
    case mRes of
      Nothing -> throw . makeAppError $ ResourceNotFound
      Just (res :: v) -> pure $ QueryResult
        -- TODO: actually handle proofs
        { queryResultData = res
        , queryResultIndex = 0
        , queryResultKey = fromBytes . makeKeyBytes . M.makeFullStoreKey m $ key
        , queryResultProof = Nothing
        , queryResultHeight = 0
        }

instance
  ( HasCodec a
  , Members QueryEffs r
  )
   => StoreQueryHandler (A.Array a) (QueryArgs Word64 -> Sem r (QueryResult a)) where
  storeQueryHandler as QueryArgs{..} = do
    let i = queryArgsData
    mRes <- as A.!! i
    case mRes of
      Nothing -> throw . makeAppError $ ResourceNotFound
      Just (res :: a) -> pure $ QueryResult
        -- TODO: actually handle proofs
        { queryResultData = res
        , queryResultIndex = 0
        , queryResultKey = fromBytes . makeKeyBytes . A.makeFullStoreKey as $ i
        , queryResultProof = Nothing
        , queryResultHeight = 0
        }

instance
  ( HasCodec a
  , Members QueryEffs r
  )
   => StoreQueryHandler (L.List a) (QueryArgs Word64 -> Sem r (QueryResult a)) where
  storeQueryHandler as QueryArgs{..} = do
    let i = queryArgsData
    mRes <- as L.!! i
    case mRes of
      Nothing -> throw . makeAppError $ ResourceNotFound
      Just (res :: a) -> pure $ QueryResult
        -- TODO: actually handle proofs
        { queryResultData = res
        , queryResultIndex = 0
        , queryResultKey = fromBytes . makeKeyBytes . L.makeFullStoreKey as $ i
        , queryResultProof = Nothing
        , queryResultHeight = 0
        }

instance
  ( HasCodec a
  , Members QueryEffs r
  )
   => StoreQueryHandler (V.Var a) (QueryArgs () -> Sem r (QueryResult a)) where
  storeQueryHandler var QueryArgs{..} = do
    mRes <- V.takeVar var
    case mRes of
      Nothing -> throw . makeAppError $ ResourceNotFound
      Just (res :: a) -> pure $ QueryResult
        -- TODO: actually handle proofs
        { queryResultData = res
        , queryResultIndex = 0
        , queryResultKey = fromBytes . makeKeyBytes . V.makeFullStoreKey $ var
        , queryResultProof = Nothing
        , queryResultHeight = 0
        }

--class StoreQueryHandlers ns r where
--    type QueryApi kvs :: *
--    storeQueryHandlers :: Proxy kvs -> Store ns -> Proxy r -> RouteQ (QueryApi kvs) r
--
--instance
--    ( IsKey k ns
--    , a ~ Value k ns
--    , HasCodec a
--    , Members QueryEffs r
--    )  => StoreQueryHandlers ns r where
--      type QueryApi (s :> StoreLeaf (M.Map k v)) =  s :> QA k :> StoreLeaf a
--      storeQueryHandlers _ store _ = storeQueryHandler (Proxy :: Proxy a) store

--instance
--    ( IsKey k ns
--    , a ~ Value k ns
--    , HasCodec a
--    , StoreQueryHandlers ((k', a') ': as) ns r
--    , Members QueryEffs r
--    ) => StoreQueryHandlers ((k,a) ': (k', a') : as) ns r where
--        type (QueryApi ((k, a) ': (k', a') : as)) = (QA k :> StoreLeaf a) :<|> QueryApi ((k', a') ': as)
--        storeQueryHandlers _ store pr =
--          storeQueryHandler  (Proxy :: Proxy a) store :<|>
--          storeQueryHandlers (Proxy :: Proxy ((k', a') ': as)) store pr
--
