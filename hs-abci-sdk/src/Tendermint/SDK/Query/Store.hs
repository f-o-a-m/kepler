{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.Query.Store where

import           Control.Error               (ExceptT, throwE)
import           Control.Lens                (to, (^.))
import           Control.Monad.Trans         (lift)
import           Data.ByteArray.Base64String (fromBytes)
import           Data.Proxy
import           GHC.TypeLits                (Symbol)
import           Polysemy                    (Members, Sem)
import           Polysemy.Error              (Error)
import           Servant.API                 ((:<|>) (..), (:>))
import           Tendermint.SDK.Codec        (HasCodec)
import           Tendermint.SDK.Errors       (AppError)
import           Tendermint.SDK.Query.Class
import           Tendermint.SDK.Query.Types
import           Tendermint.SDK.Store        (IsKey (..), RawKey (..), RawStore,
                                              StoreKey, get)

class StoreQueryHandler a (ns :: Symbol) h where
    storeQueryHandler :: Proxy a -> StoreKey ns -> h

instance
  ( IsKey k ns
  , a ~ Value k ns
  , HasCodec a
  , Members [RawStore, Error AppError] r
  )
   => StoreQueryHandler a ns (QueryArgs k -> ExceptT QueryError (Sem r) (QueryResult a)) where
  storeQueryHandler _ storeKey QueryArgs{..} = do
    let key = queryArgsData
    mRes <- lift $ get storeKey key
    case mRes of
      Nothing -> throwE ResourceNotFound
      Just (res :: a) -> pure $ QueryResult
        -- TODO: actually handle proofs
        { queryResultData = res
        , queryResultIndex = 0
        , queryResultKey = key ^. rawKey . to fromBytes
        , queryResultProof = Nothing
        , queryResultHeight = 0
        }

class StoreQueryHandlers (kvs :: [*]) (ns :: Symbol) m where
    type QueryApi kvs :: *
    storeQueryHandlers :: Proxy kvs -> StoreKey ns -> Proxy m -> RouteT (QueryApi kvs) m

instance
    ( IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , Members [RawStore, Error AppError] r
    )  => StoreQueryHandlers '[(k,a)] ns (Sem r) where
      type QueryApi '[(k,a)] =  QA k :> Leaf a
      storeQueryHandlers _ storeKey _ = storeQueryHandler (Proxy :: Proxy a) storeKey

instance
    ( IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , StoreQueryHandlers ((k', a') ': as) ns (Sem r)
    , Members [RawStore, Error AppError] r
    ) => StoreQueryHandlers ((k,a) ': (k', a') : as) ns (Sem r) where
        type (QueryApi ((k, a) ': (k', a') : as)) = (QA k :> Leaf a) :<|> QueryApi ((k', a') ': as)
        storeQueryHandlers _ storeKey pm =
          storeQueryHandler  (Proxy :: Proxy a) storeKey :<|>
          storeQueryHandlers (Proxy :: Proxy ((k', a') ': as)) storeKey pm

allStoreHandlers
  :: forall (contents :: [*]) ns r.
     StoreQueryHandlers contents ns (Sem r)
  => Proxy contents
  -> StoreKey ns
  -> Proxy r
  -> RouteT (QueryApi contents) (Sem r)
allStoreHandlers pcs storeKey _ = storeQueryHandlers pcs storeKey (Proxy :: Proxy (Sem r))
