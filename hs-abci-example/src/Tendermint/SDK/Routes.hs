module Tendermint.SDK.Routes where


import GHC.TypeLits (KnownSymbol, symbolVal)
import qualified Data.ByteString as BS
import Data.Proxy
import           URI.ByteString
import Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Types (decodePathSegments)
import Data.String.Conversions (cs)




-- For the moment it's worth just vendoring these types for
-- simplicity but there is an argument for using the actual
-- servant types for things like markdown docs etc.
data (path :: k) :> (a :: *)
infixr 4 :>

data a :<|> b = a :<|> b
infixr 3 :<|>

data Capture s a

class FromQueryData a where
    fromQueryData :: BS.ByteString -> Either String a
    fromQueryDataMaybe :: BS.ByteString -> Maybe a
    fromQueryDataMaybe = either (const Nothing) Just . fromQueryData

-- all of this was vendored from https://github.com/ElvishJerricco/servant-router
data Router m a where
  RChoice       :: Router m a -> Router m a -> Router m a
  RCapture      :: FromQueryData x => (x -> Router m a) -> Router m a
  RPath         :: KnownSymbol sym => Proxy sym -> Router m a -> Router m a
  RLeaf         :: m a -> Router m a

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

instance (HasRouter sublayout, FromQueryData x) => HasRouter (Capture sym x :> sublayout) where
  type RouteT (Capture sym x :> sublayout) m a = x -> RouteT sublayout m a
  constHandler _ m a _ = constHandler (Proxy :: Proxy sublayout) m a
  route _ m a f = RCapture (route (Proxy :: Proxy sublayout) m a . f)

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
  -> RouteT layout m a
  -> URIRef Absolute
  -> m (Either RoutingError a)
routeURI layout page uri =
  let routing = route layout Proxy Proxy page
      toMaybeQuery (k, v) = if BS.null v then (k, Nothing) else (k, Just v)

      (path, query) = case uri of
        URI{}         -> (uriPath uri, uriQuery uri)
  in  routeQueryAndPath (toMaybeQuery <$> queryPairs query) (decodePathSegments path) routing

  -- | Use a computed 'Router' to route a path and query. Generally,
-- you should use 'routeURI'.
routeQueryAndPath
  :: Monad m
  => [(BS.ByteString, Maybe BS.ByteString)]
  -> [Text]
  -> Router m a
  -> m (Either RoutingError a)
routeQueryAndPath queries pathSegs r = case r of
  RChoice a b       -> do
    result <- routeQueryAndPath queries pathSegs a
    case result of
      Left  Fail      -> routeQueryAndPath queries pathSegs b
      Left  FailFatal -> return $ Left FailFatal
      Right x         -> return $ Right x
  RCapture f        -> case pathSegs of
    [] -> return $ Left Fail
    capture:paths ->
      maybe (return $ Left FailFatal)
            (routeQueryAndPath queries paths . f)
            (fromQueryDataMaybe $ cs capture)
  RPath      sym a -> case pathSegs of
    [] -> return $ Left Fail
    p:paths ->
      if p == T.pack (symbolVal sym) then routeQueryAndPath queries paths a else return $ Left Fail
  RLeaf a          -> case pathSegs of
    [] -> Right <$> a
    _ -> return $ Left Fail