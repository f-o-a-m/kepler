module Tendermint.SDK.Router.Class where

import           Control.Monad.IO.Class        (MonadIO (..))
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
  route :: MonadIO m => Proxy layout -> Proxy m -> Delayed env (RouteT layout m) -> Router env m
  hoistRoute :: Proxy layout -> (forall a. m a -> n a) -> RouteT layout m -> RouteT layout n


instance (HasRouter a, HasRouter b) => HasRouter (a :<|> b) where
  type RouteT (a :<|> b) m = RouteT a m :<|> RouteT b m

  route _ pm server = choice (route pa pm ((\ (a :<|> _) -> a) <$> server))
                               (route pb pm ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
  hoistRoute _ phi (a :<|> b) =
    hoistRoute (Proxy :: Proxy a) phi a :<|> hoistRoute (Proxy :: Proxy b) phi b

instance (HasRouter sublayout, KnownSymbol path) => HasRouter (path :> sublayout) where

  type RouteT (path :> sublayout) m = RouteT sublayout m

  route _ pm subserver =
    pathRouter (cs (symbolVal proxyPath)) (route (Proxy :: Proxy sublayout) pm subserver)
    where proxyPath = Proxy :: Proxy path

  hoistRoute _ phi = hoistRoute (Proxy :: Proxy sublayout) phi

instance EncodeQueryResult a => HasRouter (Leaf a) where

   type RouteT (Leaf a) m = HandlerT m (QueryResult a)
   route _ _  = methodRouter
   hoistRoute _ phi = hoistHandlerT phi



instance (FromQueryData a, HasRouter layout)
      => HasRouter (QA a :> layout) where

  type RouteT (QA a :> layout) m = QueryArgs a -> RouteT layout m

  route _ pm d =
    RQueryArgs $
      route (Proxy :: Proxy layout)
          pm
          (addQueryArgs d $ \ qa -> case fromQueryData $ queryArgsData qa of
             Left e  -> delayedFail $ InvalidQuery e
             Right v -> return qa {queryArgsData = v}
          )

  hoistRoute _ phi f = hoistRoute (Proxy :: Proxy layout) phi . f
