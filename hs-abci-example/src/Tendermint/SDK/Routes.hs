module Tendermint.SDK.Routes where

import Data.ByteArray.HexString (HexString)
import Data.Int (Int64)
import Control.Monad.Reader (ReaderT, MonadReader, ask, runReaderT)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Lens ((^.), to, (&), (.~))
import GHC.TypeLits (KnownSymbol, symbolVal)
import Data.Proxy
import Data.Text (Text)
import Control.Monad (ap)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.HTTP.Types (decodePathSegments)
import Servant.API
import qualified Network.ABCI.Types.Messages.Request as Request
import qualified Network.ABCI.Types.Messages.Response  as Response
import           Network.ABCI.Types.Messages.FieldTypes (Proof)
import Data.Map (Map)
import qualified Data.Map as M
import Data.String.Conversions (cs)
import Data.Default.Class (def)
import Control.Monad.Except (ExceptT, runExceptT)


data QueryError =
    PathNotFound
  | InvalidQuery String
  | InternalError String
  deriving (Show)

--------------------------------------------------------------------------------

newtype HandlerT m a = 
  HandlerT { _runHandlerT :: ExceptT QueryError m a }
  deriving (Functor, Applicative, Monad)

runHandlerT :: HandlerT m a -> m (Either QueryError a)
runHandlerT = runExceptT . _runHandlerT

--------------------------------------------------------------------------------

data RouteResult a =
    Fail QueryError
  | FailFatal QueryError
  | Route a
  deriving (Functor)

instance Applicative RouteResult where
  pure = return
  (<*>) = ap

instance Monad RouteResult where
  return = Route
  (>>=) m f = case m of
    Route a -> f a
    Fail e -> Fail e
    FailFatal e -> FailFatal e

data RouteResultT m a = RouteResultT { runRouteResultT :: m (RouteResult a) }
  deriving (Functor)

instance MonadTrans RouteResultT where
  lift m = RouteResultT $ fmap Route m

instance Monad m => Applicative (RouteResultT m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (RouteResultT m) where
  return = RouteResultT . return . Route
  (>>=) m f = RouteResultT $ do
    a <- runRouteResultT m
    case a of 
      Route a' -> runRouteResultT $ f a' 
      Fail e -> return $ Fail e
      FailFatal e -> return $ FailFatal e

instance MonadIO m => MonadIO (RouteResultT m) where
  liftIO = lift . liftIO

--------------------------------------------------------------------------------


newtype DelayedIO a = 
  DelayedIO { runDelayedIO' :: ReaderT Request.Query (RouteResultT IO) a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader Request.Query)

liftRouteResult :: RouteResult a -> DelayedIO a
liftRouteResult x = DelayedIO $ lift  $ RouteResultT . return $ x

runDelayedIO :: DelayedIO a -> Request.Query -> IO (RouteResult a)
runDelayedIO m req = runRouteResultT $ runReaderT (runDelayedIO' m) req

--------------------------------------------------------------------------------

data Delayed env a where
  Delayed :: { delayedQueryArgs :: env -> DelayedIO qa
             , delayedHandler :: qa -> Request.Query -> RouteResult a
             } -> Delayed env a

instance Functor (Delayed env) where
  fmap f Delayed{..} = 
    Delayed { delayedHandler = \qa ->  fmap f <$> delayedHandler qa
            , ..
            }

runDelayed :: Delayed env a
           -> env
           -> Request.Query
           -> IO (RouteResult a)
runDelayed Delayed{..} env = runDelayedIO $ do
   q <- ask
   qa <- delayedQueryArgs env
   liftRouteResult $ delayedHandler qa q

runAction :: MonadIO m 
          => Delayed env (HandlerT m a)
          -> env
          -> Request.Query
          -> (a -> RouteResult Response.Query)
          -> m (RouteResult Response.Query)
runAction action env query k =
  liftIO (runDelayed action env query) >>= go 
  where 
    go (Fail e) = pure $ Fail e
    go (FailFatal e) = pure $ FailFatal e
    go (Route a) = do 
      e <- runHandlerT a
      case e of
        Left err -> pure $ Route (responseQueryError err)
        Right a' -> pure $ k a'

-- | Fail with the option to recover.
delayedFail :: QueryError -> DelayedIO a
delayedFail err = liftRouteResult $ Fail err

responseQueryError :: QueryError -> Response.Query
responseQueryError e = 
  let msg = case e of
        PathNotFound -> "Path Not Found"
        InvalidQuery m -> "Invalid Query: " <> m
        InternalError _ -> "Internal Error"
  in def { Response.queryCode = 1
         , Response.queryLog = cs msg
         }

data QueryArgs a = QueryArgs
  { queryArgsProve :: Bool
  , queryArgsData :: a
  , queryArgsQueryData :: HexString
  , queryArgsBlockHeight :: Int64
  } deriving Functor


addQueryArgs :: Delayed env (a -> b)
           -> (qa -> DelayedIO a)
           -> Delayed (qa, env) b
addQueryArgs Delayed{..} new =
  Delayed
    { delayedQueryArgs = \ (qa, env) -> (,) <$> delayedQueryArgs env <*> new qa
    , delayedHandler   = \ (x, v) query -> ($ v) <$> delayedHandler x query
    , ..
    } 

--------------------------------------------------------------------------------

data Router' env a =
    RChoice (Router' env a) (Router' env a)
  | RStatic (Map Text (Router' env a)) [env -> a]
  | RQueryArgs (Router' (QueryArgs HexString, env) a)

type RoutingApplication m = Request.Query -> m (RouteResult Response.Query)

type Router env m = Router' env (RoutingApplication m)

pathRouter :: Text -> Router' env a -> Router' env a
pathRouter t r = RStatic (M.singleton t r) []

leafRouter :: (env -> a) -> Router' env a
leafRouter l = RStatic M.empty [l]

choice :: Router' env a -> Router' env a -> Router' env a
choice (RStatic table1 ls1) (RStatic table2 ls2) =
  RStatic (M.unionWith choice table1 table2) (ls1 ++ ls2)
choice router1 (RChoice router2 router3) = RChoice (choice router1 router2) router3
choice router1 router2 = RChoice router1 router2



methodRouter
  :: MonadIO m
  => EncodeQueryResult b
  => Delayed env (HandlerT m (QueryResult b))
  -> Router env m
methodRouter action = leafRouter route'
  where
    route' env query = runAction action env query $ \QueryResult{..} ->
       Route $ def & Response._queryIndex .~ queryResultIndex
                   & Response._queryKey .~ queryResultKey
                   & Response._queryValue .~ encodeQueryResult queryResultData
                   & Response._queryProof .~ queryResultProof
                   & Response._queryHeight .~ queryResultHeight


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


class FromQueryData a where
  fromQueryData :: HexString -> Either String a


data QA (a :: *)


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


data QueryResult a = QueryResult
  { queryResultData :: a
  , queryResultIndex :: Int64
  , queryResultKey :: HexString
  , queryResultProof :: Maybe Proof
  , queryResultHeight :: Int64
  } deriving Functor

class EncodeQueryResult a where
  encodeQueryResult :: a -> HexString


data Leaf (a :: *)

instance EncodeQueryResult a => HasRouter (Leaf a) where

  type RouteT (Leaf a) m = HandlerT m (QueryResult a)
  route _ _  = methodRouter


serve 
  :: HasRouter layout
  => MonadIO m
  => Proxy layout
  -> Proxy m
  -> RouteT layout m
  -> Request.Query
  -> m Response.Query
serve p pm server = 
  toApplication (runRouter (route p pm (emptyDelayed (Route server))) ())
  where
    emptyDelayed response = 
      let r = pure ()
      in Delayed (const r) $ \_ _ -> response
    toApplication ra query = do
      res <- ra query
      case res of
        Fail e -> pure $ responseQueryError e
        FailFatal e -> pure $ responseQueryError e
        Route a -> pure a

runRouter 
  :: Monad m
  => Router env m
  -> env
  -> RoutingApplication m
runRouter router env query =
  case router of
    RStatic table ls ->
      let path = query ^. Request._queryPath . to (decodePathSegments . T.encodeUtf8)
      in case path of
        []   -> runChoice ls env query
        -- This case is to handle trailing slashes.
        [""] -> runChoice ls env query
        first : rest | Just router' <- M.lookup first table
          -> let query' = query { Request.queryPath = T.intercalate "/" rest }
             in  runRouter router' env query'
        _ -> pure $ Fail PathNotFound
    RQueryArgs r' ->
      let qa = QueryArgs
            { queryArgsData = query ^. Request._queryData
            , queryArgsQueryData = query ^. Request._queryData
            , queryArgsBlockHeight = query ^. Request._queryHeight
            , queryArgsProve = query ^. Request._queryProve
            }
      in runRouter r' (qa, env) query
    RChoice r1 r2 ->
      runChoice [runRouter r1, runRouter r2] env query
runChoice :: Monad m => [env -> RoutingApplication m] -> env -> RoutingApplication m
runChoice ls =
  case ls of
    []       -> \ _ _ -> pure $ Fail PathNotFound
    [r]      -> r
    (r : rs) ->
      \ env query -> do
        response1 <- r env query
        case response1 of
          Fail _ -> runChoice rs env query
          _      ->  pure response1

type Application m  = Request.Query -> m Response.Query