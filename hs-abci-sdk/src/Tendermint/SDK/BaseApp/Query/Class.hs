{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Query.Class where

import           Control.Error
import           Control.Monad.Morph                  (hoist)
import           Data.Proxy
import           Data.String.Conversions              (cs)
import           GHC.TypeLits                         (KnownSymbol, symbolVal)
import           Servant.API
import           Tendermint.SDK.BaseApp.Query.Delayed (Delayed, addQueryArgs,
                                                       delayedFail)
import           Tendermint.SDK.BaseApp.Query.Router  (Router, Router' (..),
                                                       choice, methodRouter,
                                                       pathRouter)
import           Tendermint.SDK.BaseApp.Query.Types   (FromQueryData (..), Leaf,
                                                       QA, QueryArgs (..),
                                                       QueryError (..),
                                                       QueryResult,
                                                       Queryable (..))

--------------------------------------------------------------------------------

-- | This class is used to construct a router given a 'layout' type. The layout
-- | is constructed using the combinators that appear in the instances here, no other
-- | Servant combinators are recognized.
class HasRouter layout where
  -- | A route handler.
  type RouteT layout (m :: * -> *) :: *
  -- | Transform a route handler into a 'Router'.
  route :: Monad m => Proxy layout -> Delayed m env (RouteT layout m) -> Router env m
  hoistRoute :: (Monad m, Monad n) => Proxy layout -> (forall a. m a -> n a) -> RouteT layout m -> RouteT layout n


instance (HasRouter a, HasRouter b) => HasRouter (a :<|> b) where
  type RouteT (a :<|> b) m = RouteT a m :<|> RouteT b m

  route _ server = choice (route pa ((\ (a :<|> _) -> a) <$> server))
                          (route pb ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

  hoistRoute _ phi (a :<|> b) =
    hoistRoute (Proxy :: Proxy a) phi a :<|>
    hoistRoute (Proxy :: Proxy b) phi b

instance (HasRouter sublayout, KnownSymbol path) => HasRouter (path :> sublayout) where

  type RouteT (path :> sublayout) m = RouteT sublayout m

  route _ subserver =
    pathRouter (cs (symbolVal proxyPath)) (route (Proxy :: Proxy sublayout) subserver)
    where proxyPath = Proxy :: Proxy path

  hoistRoute _ = hoistRoute (Proxy :: Proxy sublayout)


instance (Queryable a, KnownSymbol (Name a)) => HasRouter (Leaf a) where

   type RouteT (Leaf a) m = ExceptT QueryError m (QueryResult a)
   route _ = pathRouter (cs (symbolVal proxyPath)) . methodRouter
     where proxyPath = Proxy :: Proxy (Name a)
   hoistRoute _ = hoist



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
  hoistRoute _ phi f = hoistRoute (Proxy :: Proxy layout) phi . f
