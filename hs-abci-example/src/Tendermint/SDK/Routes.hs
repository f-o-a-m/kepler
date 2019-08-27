module Tendermint.SDK.Routes where

import Control.Lens ((^.), to)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types (decodePathSegments)
import Servant.API
import qualified Network.ABCI.Types.Messages.Request as Request
import qualified Network.ABCI.Types.Messages.Response  as Response


-- all of this was vendored from https://github.com/ElvishJerricco/servant-router
data Router m a where
  RChoice       :: Router m a -> Router m a -> Router m a
  RPath         :: KnownSymbol sym => Proxy sym -> Router m a -> Router m a
  RLeaf         :: (Request.Query -> m Response.Query) -> Router m Response.Query

class HasRouter layout where
  -- | A route handler.
  type RouteT layout (m :: * -> *) a :: *
  -- | Create a constant route handler that returns @a@
  constHandler :: Monad m => Proxy layout -> Proxy m -> a -> RouteT layout m a
  -- | Transform a route handler into a 'Router'.
  route :: Proxy layout -> Proxy m -> Proxy a -> RouteT layout m a -> Router m a

instance (HasRouter x, HasRouter y) => HasRouter (x :<|> y) where
  type RouteT (x :<|> y) m a = RouteT x m a :<|> RouteT y m a
  constHandler _ m a = constHandler (Proxy :: Proxy x) m a
                  :<|> constHandler (Proxy :: Proxy y) m a
  route
    _
    (m :: Proxy m)
    (a :: Proxy a)
    ((x :: RouteT x m a) :<|> (y :: RouteT y m a))
    = RChoice (route (Proxy :: Proxy x) m a x) (route (Proxy :: Proxy y) m a y)

instance (HasRouter sublayout, KnownSymbol path) => HasRouter (path :> sublayout) where
  type RouteT (path :> sublayout) m a = RouteT sublayout m a
  constHandler _ = constHandler (Proxy :: Proxy sublayout)
  route _ m a page = RPath
    (Proxy :: Proxy path)
    (route (Proxy :: Proxy sublayout) m a page)

data RoutingError = Fail | FailFatal deriving (Show, Eq, Ord)


-- | Use a handler to route a 'URIRef'.
routeURI
  :: (HasRouter layout, Monad m)
  => Proxy layout
  -> RouteT layout m Response.Query
  -> Request.Query
  -> m (Either RoutingError Response.Query)
routeURI layout page query =
  let routing = route layout Proxy Proxy page
      path = query ^. Request._queryPath . to (decodePathSegments . T.encodeUtf8)
  in  routeQueryAndPath query path routing

  -- | Use a computed 'Router' to route a path and query. Generally,
-- you should use 'routeURI'.
routeQueryAndPath
  :: Monad m
  => Request.Query
  -> [Text]
  -> Router m Response.Query
  -> m (Either RoutingError Response.Query)
routeQueryAndPath query pathSegs r = case r of
  RChoice a b       -> do
    result <- routeQueryAndPath query pathSegs a
    case result of
      Left  Fail      -> routeQueryAndPath query pathSegs b
      Left  FailFatal -> return $ Left FailFatal
      Right x         -> return $ Right x
  RPath      sym a -> case pathSegs of
    [] -> return $ Left Fail
    p:paths ->
      if p == T.pack (symbolVal sym) then routeQueryAndPath query paths a else return $ Left Fail
  RLeaf f          -> case pathSegs of
    [] -> Right <$> f query
    _ -> return $ Left Fail