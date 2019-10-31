module Tendermint.SDK.Router.Class where

import           Control.Error
import           Data.Proxy
import           Data.String.Conversions       (cs)
import           GHC.TypeLits                  (KnownSymbol, symbolVal)
import           Servant.API
import           Tendermint.SDK.Router.Delayed
import           Tendermint.SDK.Router.Router
import           Tendermint.SDK.Router.Types

--------------------------------------------------------------------------------


class HasRouter layout where
  -- | A route handler.
  type RouteT layout (m :: * -> *) :: *
  -- | Transform a route handler into a 'Router'.
  route :: Monad m => Proxy layout -> Delayed m env (RouteT layout m) -> Router env m


instance (HasRouter a, HasRouter b) => HasRouter (a :<|> b) where
  type RouteT (a :<|> b) m = RouteT a m :<|> RouteT b m

  route _ server = choice (route pa ((\ (a :<|> _) -> a) <$> server))
                          (route pb ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance (HasRouter sublayout, KnownSymbol path) => HasRouter (path :> sublayout) where

  type RouteT (path :> sublayout) m = RouteT sublayout m

  route _ subserver =
    pathRouter (cs (symbolVal proxyPath)) (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path


instance EncodeQueryResult a => HasRouter (Leaf a) where

   type RouteT (Leaf a) m = ExceptT QueryError m (QueryResult a)
   route _  = methodRouter


instance (FromQueryData a, HasRouter layout)
      => HasRouter (QA a :> layout) where

  type RouteT (QA a :> layout) m = QueryArgs a -> RouteT layout m

  route _ d =
    RQueryArgs $
      route (Proxy :: Proxy layout)
          (addQueryArgs d $ \ qa -> case fromQueryData $ queryArgsData qa of
             Left e  -> delayedFail $ InvalidQuery e
             Right v -> return qa {queryArgsData = v}
          )
