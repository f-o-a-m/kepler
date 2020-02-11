{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.BaseApp.Query.Store where

import           Control.Lens                        (to, (^.))
import           Data.ByteArray.Base64String         (fromBytes)
import           Data.Proxy
import           Data.String.Conversions             (cs)
import           GHC.TypeLits                        (KnownSymbol, Symbol,
                                                      symbolVal)
import           Polysemy                            (Member, Members, Sem)
import           Polysemy.Error                      (throw)
import           Polysemy.Tagged                     (Tagged)
import           Servant.API                         ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Errors       (makeAppError)
import           Tendermint.SDK.BaseApp.Query.Effect (QueryEffs)
import           Tendermint.SDK.BaseApp.Query.Router (HasQueryRouter (..),
                                                      methodRouter)
import           Tendermint.SDK.BaseApp.Query.Types  (Leaf, QA, QueryArgs (..),
                                                      QueryResult (..),
                                                      Queryable (..))
import           Tendermint.SDK.BaseApp.Router       (RouterError (..),
                                                      pathRouter)
import           Tendermint.SDK.BaseApp.Store        (IsKey (..), RawKey (..),
                                                      ReadStore, Scope (..),
                                                      StoreKey, get)
import           Tendermint.SDK.Codec                (HasCodec)

data StoreLeaf a

instance (Queryable a,  Member (Tagged 'QueryAndMempool ReadStore) r, KnownSymbol (Name a)) => HasQueryRouter (StoreLeaf a) r where

   type RouteQ (StoreLeaf a) r = Sem r (QueryResult a)
   routeQ _ _ = pathRouter (cs (symbolVal proxyPath)) . methodRouter
     where proxyPath = Proxy :: Proxy (Name a)
   hoistQueryRouter _ _ = ($)

class StoreQueryHandler a (ns :: Symbol) h where
    storeQueryHandler :: Proxy a -> StoreKey ns -> h

instance
  ( IsKey k ns
  , a ~ Value k ns
  , HasCodec a
  , Members QueryEffs r
  )
   => StoreQueryHandler a ns (QueryArgs k -> Sem r (QueryResult a)) where
  storeQueryHandler _ storeKey QueryArgs{..} = do
    let key = queryArgsData
    mRes <- get storeKey key
    case mRes of
      Nothing -> throw . makeAppError $ ResourceNotFound
      Just (res :: a) -> pure $ QueryResult
        -- TODO: actually handle proofs
        { queryResultData = res
        , queryResultIndex = 0
        , queryResultKey = key ^. rawKey . to fromBytes
        , queryResultProof = Nothing
        , queryResultHeight = 0
        }

class StoreQueryHandlers (kvs :: [*]) (ns :: Symbol) r where
    type QueryApi kvs :: *
    storeQueryHandlers :: Proxy kvs -> StoreKey ns -> Proxy r -> RouteQ (QueryApi kvs) r

instance
    ( IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , Members QueryEffs r
    )  => StoreQueryHandlers '[(k,a)] ns r where
      type QueryApi '[(k,a)] =  QA k :> StoreLeaf a
      storeQueryHandlers _ storeKey _ = storeQueryHandler (Proxy :: Proxy a) storeKey

instance
    ( IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , StoreQueryHandlers ((k', a') ': as) ns r
    , Members QueryEffs r
    ) => StoreQueryHandlers ((k,a) ': (k', a') : as) ns r where
        type (QueryApi ((k, a) ': (k', a') : as)) = (QA k :> Leaf a) :<|> QueryApi ((k', a') ': as)
        storeQueryHandlers _ storeKey pr =
          storeQueryHandler  (Proxy :: Proxy a) storeKey :<|>
          storeQueryHandlers (Proxy :: Proxy ((k', a') ': as)) storeKey pr
