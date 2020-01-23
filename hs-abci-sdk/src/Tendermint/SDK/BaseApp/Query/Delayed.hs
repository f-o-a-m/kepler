module Tendermint.SDK.BaseApp.Query.Delayed where

import           Control.Monad.Reader                 (MonadReader, ReaderT,
                                                       ask, runReaderT)
import           Control.Monad.Trans                  (MonadTrans (..))
import qualified Network.ABCI.Types.Messages.Response as Response
import           Polysemy                             (Sem)
import           Tendermint.SDK.BaseApp.Query.Types   (QueryArgs (..),
                                                       QueryError (..),
                                                       QueryRequest (..),
                                                       RouteResult (..),
                                                       RouteResultT (..),
                                                       defaultQueryWithData)

--------------------------------------------------------------------------------
-- NOTE: most of this was vendored and repurposed from servant


newtype DelayedM m a =
  DelayedM { runDelayedM' :: ReaderT QueryRequest (RouteResultT m) a }
    deriving (Functor, Applicative, Monad, MonadReader QueryRequest)

liftRouteResult :: Monad m => RouteResult a -> DelayedM m a
liftRouteResult x = DelayedM $ lift $ RouteResultT . return $ x

runDelayedM :: DelayedM m a -> QueryRequest -> m (RouteResult a)
runDelayedM m req = runRouteResultT $ runReaderT (runDelayedM' m) req

--------------------------------------------------------------------------------

data Delayed m env a where
  Delayed :: { delayedCaptures :: env -> DelayedM m captures
             , delayedQueryArgs :: DelayedM m qa
             , delayedParams :: DelayedM m params
             , delayedHandler :: captures -> qa -> params -> QueryRequest -> RouteResult a
             } -> Delayed m env a

instance Functor m => Functor (Delayed m env) where
  fmap f Delayed{..} =
    Delayed { delayedHandler = \captures qa params q -> f <$> delayedHandler captures qa params q
            , ..
            }

runDelayed
  :: Monad m
  => Delayed m env a
  -> env
  -> QueryRequest
  -> m (RouteResult a)
runDelayed Delayed{..} env = runDelayedM (do
    q <- ask
    captures <- delayedCaptures env
    qa <- delayedQueryArgs
    params <- delayedParams
    liftRouteResult $ delayedHandler captures qa params q
  )

runAction
  :: Delayed (Sem r) env (Sem r a)
  -> env
  -> QueryRequest
  -> (a -> RouteResult Response.Query)
  -> Sem r (RouteResult Response.Query)
runAction action env query k = do
    res <- runDelayed action env query
    case res of
      Route a     -> k <$> a
      Fail e      -> pure $ Fail e
      FailFatal e -> pure $ FailFatal e

-- | Fail with the option to recover.
delayedFail :: Monad m => QueryError -> DelayedM m a
delayedFail err = liftRouteResult $ Fail err

addQueryArgs
  :: Monad m
  => Delayed m env (QueryArgs a -> b)
  -> DelayedM m (QueryArgs a)
  -> Delayed m env b
addQueryArgs Delayed{..} new =
  Delayed
    { delayedQueryArgs = (,) <$> delayedQueryArgs <*> new
    , delayedHandler = \caps (qa, qaNew) p query -> ($ qaNew) <$> delayedHandler caps qa p query
    , ..
    }

addCapture
  :: Monad m
  => Delayed m env (a -> b)
  -> (captured -> DelayedM m a)
  -> Delayed m (captured, env) b
addCapture Delayed{..} new =
  Delayed
    { delayedCaptures = \ (txt, env) -> (,) <$> delayedCaptures env <*> new txt
    , delayedHandler   = \ (x, v) qa p query -> ($ v) <$> delayedHandler x qa p query
    , ..
    } -- Note [Existential Record Update]

addParameter
  :: Monad m
  => Delayed m env (a -> b)
  -> DelayedM m a
  -> Delayed m env b
addParameter Delayed {..} new =
  Delayed
    { delayedParams = (,) <$> delayedParams <*> new
    , delayedHandler = \caps qa (p, pNew) query -> ($ pNew) <$> delayedHandler caps qa p query
    , ..
    }

emptyDelayed :: Monad m => RouteResult a -> Delayed m b a
emptyDelayed response =
  let r = pure ()
      qa = pure $ defaultQueryWithData ()
  in Delayed (const r) qa r $ \_ _ _ _ -> response

-- | Gain access to the incoming request.
withQuery
  :: Monad m
  => (QueryRequest -> DelayedM m a)
  -> DelayedM m a
withQuery f = do
  req <- ask
  f req