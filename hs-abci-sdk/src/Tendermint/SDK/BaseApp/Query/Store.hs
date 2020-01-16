{-# LANGUAGE UndecidableInstances #-}

module Tendermint.SDK.BaseApp.Query.Store where

import           Control.Lens                       (to, (^.))
import           Data.ByteArray.Base64String        (fromBytes)
import           Data.Proxy
import           GHC.TypeLits                       (Symbol)
import           Polysemy                           (Members, Sem)
import           Polysemy.Error                     (Error, throw)
import           Servant.API                        ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Errors      (AppError, makeAppError)
import           Tendermint.SDK.BaseApp.Query.Class
import           Tendermint.SDK.BaseApp.Query.Types
import           Tendermint.SDK.BaseApp.Store       (IsKey (..), RawKey (..),
                                                     RawStore, StoreKey, get)
import           Tendermint.SDK.Codec               (HasCodec)

class StoreQueryHandler a (ns :: Symbol) h where
    storeQueryHandler :: Proxy a -> StoreKey ns -> h

instance
  ( IsKey k ns
  , a ~ Value k ns
  , HasCodec a
  , Members [RawStore, Error AppError] r
  )
   => StoreQueryHandler a ns (QueryArgs k -> (Sem r) (QueryResult a)) where
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
    storeQueryHandlers :: Proxy kvs -> StoreKey ns -> Proxy r -> RouteT (QueryApi kvs) r

instance
    ( IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , Members [RawStore, Error AppError] r
    )  => StoreQueryHandlers '[(k,a)] ns r where
      type QueryApi '[(k,a)] =  QA k :> Leaf a
      storeQueryHandlers _ storeKey _ = storeQueryHandler (Proxy :: Proxy a) storeKey

instance
    ( IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , StoreQueryHandlers ((k', a') ': as) ns r
    , Members [RawStore, Error AppError] r
    ) => StoreQueryHandlers ((k,a) ': (k', a') : as) ns r where
        type (QueryApi ((k, a) ': (k', a') : as)) = (QA k :> Leaf a) :<|> QueryApi ((k', a') ': as)
        storeQueryHandlers _ storeKey pr =
          storeQueryHandler  (Proxy :: Proxy a) storeKey :<|>
          storeQueryHandlers (Proxy :: Proxy ((k', a') ': as)) storeKey pr

allStoreHandlers
  :: forall (contents :: [*]) ns r.
     StoreQueryHandlers contents ns r
  => Proxy contents
  -> StoreKey ns
  -> Proxy r
  -> RouteT (QueryApi contents) r
allStoreHandlers pcs storeKey pr = storeQueryHandlers pcs storeKey pr
