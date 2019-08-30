module Tendermint.SDK.Router.Class where

import Control.Monad.IO.Class (MonadIO(..))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy
import Servant.API
import Data.String.Conversions (cs)
import Tendermint.SDK.Router.Types
import Tendermint.SDK.Router.Delayed
import Tendermint.SDK.Router.Router

--------------------------------------------------------------------------------


class HasRouter layout where
  -- | A route handler.
  type RouteT layout (m :: * -> *) :: *
  -- | Transform a route handler into a 'Router'.
  route :: MonadIO m => Proxy layout -> Proxy m -> Delayed env (RouteT layout m) -> Router env m
  

instance (HasRouter a, HasRouter b) => HasRouter (a :<|> b) where
  type RouteT (a :<|> b) m = RouteT a m :<|> RouteT b m

  route _ pm server = choice (route pa pm ((\ (a :<|> _) -> a) <$> server))
                               (route pb pm ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b
  
instance (HasRouter sublayout, KnownSymbol path) => HasRouter (path :> sublayout) where

  type RouteT (path :> sublayout) m = RouteT sublayout m

  route _ pm subserver = 
    pathRouter (cs (symbolVal proxyPath)) (route (Proxy :: Proxy sublayout) pm subserver)
    where proxyPath = Proxy :: Proxy path

instance EncodeQueryResult a => HasRouter (Leaf a) where
 
   type RouteT (Leaf a) m = HandlerT m (QueryResult a)
   route _ _  = methodRouter
 


instance (FromQueryData a, HasRouter layout)
      => HasRouter (QA a :> layout) where

  type RouteT (QA a :> layout) m = QueryArgs a -> RouteT layout m

  route _ pm d =
    RQueryArgs $
      route (Proxy :: Proxy layout)
          pm
          (addQueryArgs d $ \ qa -> case fromQueryData $ queryArgsData qa of
             Left e -> delayedFail $ InvalidQuery e
             Right v  -> return qa {queryArgsData = v}
          )
