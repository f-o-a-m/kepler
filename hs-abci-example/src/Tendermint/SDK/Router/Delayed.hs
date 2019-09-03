module Tendermint.SDK.Router.Delayed where

import           Control.Monad.IO.Class               (MonadIO (..))
import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       ask, runReaderT)
import           Control.Monad.Trans                  (MonadTrans (..))
import           Data.Default.Class                   (def)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Tendermint.SDK.Router.Types

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
    Delayed { delayedHandler = fmap (fmap f) . delayedHandler
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
        PathNotFound    -> "Path Not Found"
        ResourceNotFound -> "Resource Not Found"
        InvalidQuery m  -> "Invalid Query: " <> m
        InternalError _ -> "Internal Error"
  in def { Response.queryCode = 1
         , Response.queryLog = cs msg
         }

addQueryArgs :: Delayed env (a -> b)
           -> (qa -> DelayedIO a)
           -> Delayed (qa, env) b
addQueryArgs Delayed{..} new =
  Delayed
    { delayedQueryArgs = \ (qa, env) -> (,) <$> delayedQueryArgs env <*> new qa
    , delayedHandler   = \ (x, v) query -> ($ v) <$> delayedHandler x query
    , ..
    }
