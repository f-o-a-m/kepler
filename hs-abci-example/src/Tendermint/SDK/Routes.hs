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
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Conversions (cs)
import Data.Default.Class (def)
import Control.Monad.Except (ExceptT, runExceptT)


data QueryError =
    PathNotFound
  | InvalidQueryData String 
  | InternalError String
  deriving (Show)

-- all of this was vendored from https://github.com/ElvishJerricco/servant-router
data Router' a =
    RChoice (Router' a) (Router' a)
  | RStatic (Map Text (Router' a)) [a]

type RoutingApplication m = Request.Query -> m (RouteResult Response.Query)

type Router m = Router' (RoutingApplication m)

pathRouter :: Text -> Router' a -> Router' a
pathRouter t r = RStatic (M.singleton t r) []

leafRouter :: a -> Router' a
leafRouter l = RStatic M.empty [l]

choice :: Router' a -> Router' a -> Router' a
choice (RStatic table1 ls1) (RStatic table2 ls2) =
  RStatic (M.unionWith choice table1 table2) (ls1 ++ ls2)
choice router1 (RChoice router2 router3) = RChoice (choice router1 router2) router3
choice router1 router2 = RChoice router1 router2

data RouteResult a =
    Fail QueryError
  | FailFatal QueryError
  | Route a
  deriving (Functor)

data Delayed a = Delayed (Request.Query -> RouteResult a)

instance Functor Delayed where
  fmap f (Delayed g) = Delayed (fmap f . g)

runDelayed :: Delayed a
           -> Request.Query
           -> RouteResult a
runDelayed (Delayed action) query = action query


runAction :: Monad m 
          => Delayed (ExceptT QueryError m a)
          -> Request.Query
          -> (RouteResult a -> RouteResult Response.Query)
          -> m (RouteResult Response.Query)
runAction action req respond = 
    go $ runDelayed action req
  where
    go (Fail e) = pure $ respond (Fail e)
    go (FailFatal e) = pure $ respond (FailFatal e)
    go (Route ma) = do
      eRes <- runExceptT ma
      pure $ respond $ 
        case eRes of
           Left err -> Fail err
           Right res -> Route res


methodRouter
  :: Monad m
  => Delayed (ExceptT QueryError m b)
  -> Router m
methodRouter action = leafRouter route'
  where
    route' request = runAction action request $ \_ -> Route def

class HasRouter layout where
  -- | A route handler.
  type RouteT layout (m :: * -> *) :: *
  -- | Transform a route handler into a 'Router'.
  route :: Monad m => Proxy layout -> Proxy m -> Delayed (RouteT layout m) -> Router m
  

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

data Leaf (a :: *)

instance HasRouter (Leaf a) where

  type RouteT (Leaf a) m = ExceptT QueryError m a
  route _ _  = methodRouter


serve 
  :: HasRouter layout
  => Monad m
  => Proxy layout
  -> Proxy m
  -> RouteT layout m
  -> Request.Query
  -> m Response.Query
serve p pm server query = 
  toApplication (runRouter (route p pm (emptyDelayed (Route server))))
  where
    emptyDelayed response = Delayed $ const response
    toApplication ra = do
      res <- ra query
      case res of
        Fail _ -> pure def
        FailFatal _ -> pure def
        Route a -> pure a

runRouter 
  :: Monad m
  => Router m
  -> RoutingApplication m
runRouter router query =
  case router of
    RStatic table ls ->
      let path = query ^. Request._queryPath . to (decodePathSegments . T.encodeUtf8)
      in case path of
        []   -> runChoice ls query
        -- This case is to handle trailing slashes.
        [""] -> runChoice ls query
        first : rest | Just router' <- M.lookup first table
          -> let query' = query { Request.queryPath = T.intercalate "/" rest }
             in  runRouter router' query'
        _ -> pure $ Fail PathNotFound
    RChoice r1 r2 ->
      runChoice [runRouter r1, runRouter r2] query
runChoice :: Monad m => [RoutingApplication m] -> RoutingApplication m
runChoice ls =
  case ls of
    []       -> \ _ -> pure $ Fail PathNotFound
    [r]      -> r
    (r : rs) ->
      \ query -> do
        response1 <- r query
        case response1 of
          Fail _ -> runChoice rs query
          _      ->  pure response1