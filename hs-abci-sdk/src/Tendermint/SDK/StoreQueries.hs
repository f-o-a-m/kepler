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
import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Types
import           Tendermint.SDK.Store        (HasKey (..), RawStore, get)

class StoreQueryHandler a h where
    storeQueryHandler :: Proxy a -> h

instance
  ( HasKey a
  , Key a ~ k
  , Member RawStore r
  )
   => StoreQueryHandler a (QueryArgs k -> ExceptT QueryError (Sem r) (QueryResult a)) where
  storeQueryHandler _ QueryArgs{..} = do
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

class StoreQueryHandlers (items :: [*]) m where
    type QueryApi items :: *
    storeQueryHandlers :: Proxy items -> Proxy m -> RouteT (QueryApi items) m

instance
    ( Queryable a
    , Member RawStore r
    )  => StoreQueryHandlers (a ': '[]) (Sem r) where
      type QueryApi (a ': '[])  =  Name a :> QA (Key a) :> Leaf a
      storeQueryHandlers _ _ = storeQueryHandler (Proxy :: Proxy a)

instance
    ( Queryable a
    , StoreQueryHandlers (a': as) (Sem r)
    , Member RawStore r
    ) => StoreQueryHandlers (a ': a' : as) (Sem r) where
        type (QueryApi (a ': a' : as)) = (Name a :> QA (Key a) :> Leaf a) :<|> QueryApi (a' ': as)
        storeQueryHandlers _ pm =
          storeQueryHandler  (Proxy :: Proxy a) :<|>
          storeQueryHandlers (Proxy :: Proxy (a' ': as)) pm

allStoreHandlers
  :: forall (contents :: [*]) r.
     StoreQueryHandlers contents (Sem r)
  => Member RawStore r
  => Proxy contents
  -> Proxy r
  -> RouteT (QueryApi contents) (Sem r)
allStoreHandlers _ _ = storeQueryHandlers (Proxy :: Proxy contents) (Proxy :: Proxy (Sem r))
