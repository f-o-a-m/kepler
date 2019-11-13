module Tendermint.SDK.StoreQueries where

--import Servant.API
-- import Tendermint.SDK.Routes
import           Control.Error               (ExceptT, throwE)
import           Control.Lens                (to, (^.))
import           Control.Monad.Trans         (lift)
import           Data.ByteArray.Base64String (fromBytes)
import           Data.Proxy
import           GHC.TypeLits                (Symbol)
import           Polysemy                    (Member, Sem)
import           Servant.API                 ((:<|>) (..), (:>))
import           Tendermint.SDK.Codec        (HasCodec)
import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Types
import           Tendermint.SDK.Store        (IsKey (..), RawKey (..), RawStore,
                                              get)

class StoreQueryHandler a (ns :: Symbol) h where
    storeQueryHandler :: Proxy a -> Proxy ns -> h

instance
  ( IsKey k ns
  , a ~ Value k ns
  , HasCodec a
  , Member RawStore r
  )
   => StoreQueryHandler a ns (QueryArgs k -> ExceptT QueryError (Sem r) (QueryResult a)) where
  storeQueryHandler _ _ QueryArgs{..} = do
    let key = queryArgsData
    mRes <- lift $ get undefined key
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
    storeQueryHandlers :: Proxy kvs -> Proxy ns -> Proxy m -> RouteT (QueryApi kvs) m

instance
    ( Queryable a
    , IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , Member RawStore r
    )  => StoreQueryHandlers '[(k,a)] ns (Sem r) where
      type QueryApi '[(k,a)] =  Name a :> QA k :> Leaf a
      storeQueryHandlers _ pns _ = storeQueryHandler (Proxy :: Proxy a) pns

instance
    ( Queryable a
    , IsKey k ns
    , a ~ Value k ns
    , HasCodec a
    , StoreQueryHandlers ((k', a') ': as) ns (Sem r)
    , Member RawStore r
    ) => StoreQueryHandlers ((k,a) ': (k', a') : as) ns (Sem r) where
        type (QueryApi ((k, a) ': (k', a') : as)) = (Name a :> QA k :> Leaf a) :<|> QueryApi ((k', a') ': as)
        storeQueryHandlers _ pns pm =
          storeQueryHandler  (Proxy :: Proxy a) pns :<|>
          storeQueryHandlers (Proxy :: Proxy ((k', a') ': as)) pns pm

allStoreHandlers
  :: forall (contents :: [*]) ns r.
     StoreQueryHandlers contents ns (Sem r)
  => Member RawStore r
  => Proxy contents
  -> Proxy ns
  -> Proxy r
  -> RouteT (QueryApi contents) (Sem r)
allStoreHandlers pcs pns _ = storeQueryHandlers pcs pns (Proxy :: Proxy (Sem r))
