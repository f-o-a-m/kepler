module Tendermint.SDK.StoreQueries where

--import Servant.API
-- import Tendermint.SDK.Routes
import           Control.Lens                (to, (^.))
import           Control.Monad.Except        (throwError)
import           Control.Monad.Trans         (lift)
import           Data.ByteArray.Base64String (fromBytes)
import           Data.Proxy
import           Servant.API
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Types
import           Tendermint.SDK.Store


class StoreQueryHandler a store h where
    storeQueryHandler :: Proxy a -> store -> h

instance (HasKey a, Key a ~ k, ContainsCodec a contents, Monad m)
   => StoreQueryHandler a (Store contents m) (QueryArgs k -> HandlerT m (QueryResult a)) where
  storeQueryHandler _ store QueryArgs{..} = do
    let key = queryArgsData
    mRes <- lift $ get (Root mempty) key store
    case mRes of
      Nothing -> throwError ResourceNotFound
      Just (res :: a) -> pure $ QueryResult
        -- TODO: actually handle proofs
        { queryResultData = res
        , queryResultIndex = 0
        , queryResultKey = key ^. rawKey . to fromBytes
        , queryResultProof = Nothing
        , queryResultHeight = 0
        }

class StoreQueryHandlers (items :: [*]) (contents :: [*]) m where
    type QueryApi items :: *
    storeQueryHandlers :: Proxy items -> Store contents m -> RouteT (QueryApi items) m

instance
    ( HasKey a
    , Monad m
    , HasCodec a
    , ContainsCodec a contents
    , Queryable a
    )  => StoreQueryHandlers (a ': '[]) contents m where
      type QueryApi (a ': '[])  =  Name a :> QA (Key a) :> Leaf a
      storeQueryHandlers _ store = storeQueryHandler  (Proxy :: Proxy a) store

instance
    ( HasKey a
    , Monad m
    , HasCodec a
    , Queryable a
    , ContainsCodec a contents
    , StoreQueryHandlers (a': as) contents m
    ) => StoreQueryHandlers (a ': a' : as) contents m where
        type (QueryApi (a ': a' : as)) = (Name a :> QA (Key a) :> Leaf a) :<|> QueryApi (a' ': as)
        storeQueryHandlers _ store =
          storeQueryHandler  (Proxy :: Proxy a) store :<|>
          storeQueryHandlers (Proxy :: Proxy (a' ': as)) store

allStoreHandlers
  :: forall (contents :: [*]) m .
     StoreQueryHandlers contents contents m
  => Monad m
  => Store contents m
  -> RouteT (QueryApi contents) m
allStoreHandlers = storeQueryHandlers (Proxy :: Proxy contents)
