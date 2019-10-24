module Tendermint.SDK.Router.Delayed where

import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       ask, runReaderT)
import           Control.Monad.Trans                  (MonadTrans (..))
import           Data.Default.Class                   (def)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Tendermint.SDK.Router.Types
import Control.Error (ExceptT, runExceptT)

--------------------------------------------------------------------------------


newtype DelayedM m a =
  DelayedM { runDelayedM' :: ReaderT Request.Query (RouteResultT m) a }
    deriving (Functor, Applicative, Monad, MonadReader Request.Query)

liftRouteResult :: Monad m => RouteResult a -> DelayedM m a
liftRouteResult x = DelayedM $ lift $ RouteResultT . return $ x

runDelayedM :: DelayedM m a -> Request.Query -> m (RouteResult a)
runDelayedM m req = runRouteResultT $ runReaderT (runDelayedM' m) req

--------------------------------------------------------------------------------

data Delayed m env a where
  Delayed :: { delayedQueryArgs :: env -> DelayedM m qa
             , delayedHandler :: qa -> Request.Query -> RouteResult a
             } -> Delayed m env a

instance Functor (Delayed m env) where
  fmap f Delayed{..} =
    Delayed { delayedHandler = fmap (fmap f) . delayedHandler
            , ..
            }

runDelayed :: Monad m
           => Delayed m env a
           -> env
           -> Request.Query
           -> m (RouteResult a)
runDelayed Delayed{..} env = runDelayedM (do
    q <- ask
    qa <- delayedQueryArgs env
    liftRouteResult $ delayedHandler qa q
  )

runAction :: Monad m
          => Delayed m env (ExceptT QueryError m a)
          -> env
          -> Request.Query
          -> (a -> RouteResult Response.Query)
          -> m (RouteResult Response.Query)
runAction action env query k =
  runDelayed action env query >>= go
  where
    go (Fail e) = pure $ Fail e
    go (FailFatal e) = pure $ FailFatal e
    go (Route a) = do
      e <- runExceptT a
      case e of
        Left err -> pure $ Route (responseQueryError err)
        Right a' -> pure $ k a'

-- | Fail with the option to recover.
delayedFail :: Monad m => QueryError -> DelayedM m a
delayedFail err = liftRouteResult $ Fail err

responseQueryError :: QueryError -> Response.Query
responseQueryError e =
  let msg = case e of
        PathNotFound     -> "Path Not Found"
        ResourceNotFound -> "Resource Not Found"
        InvalidQuery m   -> "Invalid Query: " <> m
        InternalError _  -> "Internal Error"
  in def { Response.queryCode = 1
         , Response.queryLog = cs msg
        }

addQueryArgs
  :: Monad m
  => Delayed m env (a -> b)
  -> (qa -> DelayedM m a)
  -> Delayed m (qa, env) b
addQueryArgs Delayed{..} new =
  Delayed
    { delayedQueryArgs = \ (qa, env) -> (,) <$> delayedQueryArgs env <*> new qa
    , delayedHandler   = \ (x, v) query -> ($ v) <$> delayedHandler x query
    , ..
    }

emptyDelayed :: Monad m => RouteResult a -> Delayed m b a
emptyDelayed response =
  let r = pure ()
  in Delayed (const r) $ \_ _ -> response
