{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.StoreQueries where

--import Servant.API
-- import Tendermint.SDK.Routes
import           Control.Error               (ExceptT, throwE)
import           Control.Lens                (to, (^.))
import           Control.Monad.Trans         (lift)
import           Data.ByteArray.Base64String (fromBytes)
import           Data.Proxy
import           Polysemy                    (Member, Sem)
import           Servant.API                 ((:<|>) (..), (:>))
import           Tendermint.SDK.Codec        (HasCodec)
import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Types
import           Tendermint.SDK.Store        (IsKey (..), IsRawKey (..), Store,
                                              get)

class StoreQueryHandler a h where
    storeQueryHandler :: Proxy a -> h

instance
  ( IsKey storeKey k
  , Value storeKey k ~ a
  , HasCodec a
  , Member (Store storeKey) r
  )
   => StoreQueryHandler a (QueryArgs k -> ExceptT QueryError (Sem r) (QueryResult a)) where
  storeQueryHandler _ QueryArgs{..} = do
    let key = queryArgsData
    mRes <- lift $ get key
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

class StoreQueryHandlers (kvs :: [*]) m where
    type QueryApi kvs :: *
    storeQueryHandlers :: Proxy kvs -> Proxy m -> RouteT (QueryApi kvs) m

instance forall k a r storeKey.
    ( Queryable a
    , HasCodec a
    , IsKey storeKey k
    , Value storeKey k ~ a
    , Member (Store storeKey) r
    )  => StoreQueryHandlers '[(k,a)] (Sem r) where
      type QueryApi '[(k,a)] =  Name a :> QA k :> Leaf a
      storeQueryHandlers _ _ = storeQueryHandler (Proxy :: Proxy a)

instance
    ( Queryable a
    , IsKey storeKey k
    , Value storeKey k ~ a
    , HasCodec a
    , StoreQueryHandlers  ((k',a') ': kas) (Sem r)
    , Member (Store storeKey) r
    ) => StoreQueryHandlers ((k, a) ': (k', a') ': kas) (Sem r) where
        type (QueryApi ((k, a) ': (k', a') ': kas)) = (Name a :> QA k :> Leaf a) :<|> QueryApi ((k',a') ': kas)
        storeQueryHandlers _ pm =
          storeQueryHandler  (Proxy :: Proxy a) :<|>
          storeQueryHandlers (Proxy :: Proxy ((k',a') ': kas)) pm

allStoreHandlers
  :: forall (kvs :: [*]) storeKey r.
     StoreQueryHandlers kvs (Sem r)
  => Member (Store storeKey) r
  => Proxy kvs
  -> Proxy r
  -> RouteT (QueryApi kvs) (Sem r)
allStoreHandlers pkvs _ = storeQueryHandlers pkvs (Proxy :: Proxy (Sem r))
