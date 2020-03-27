module Tendermint.SDK.BaseApp.Router.Delayed
  ( Delayed
  , runAction
  , delayedFail
  , addBody
  , addCapture
  , addParameter
  , emptyDelayed
  , withRequest
  ) where

import           Control.Monad.Reader                (MonadReader, ReaderT, ask,
                                                      runReaderT)
import           Control.Monad.Trans                 (MonadTrans (..))
import           Polysemy                            (Sem)
import           Tendermint.SDK.BaseApp.Router.Types (RouteResult (..),
                                                      RouteResultT (..),
                                                      RouterError (..))

--------------------------------------------------------------------------------
-- NOTE: most of this was vendored and repurposed from servant


newtype DelayedM m req a =
  DelayedM { runDelayedM' :: ReaderT req (RouteResultT m) a }
    deriving (Functor, Applicative, Monad, MonadReader req)

liftRouteResult :: Monad m => RouteResult a -> DelayedM m req a
liftRouteResult x = DelayedM $ lift $ RouteResultT . return $ x

runDelayedM :: DelayedM m req a -> req -> m (RouteResult a)
runDelayedM m req = runRouteResultT $ runReaderT (runDelayedM' m) req

--------------------------------------------------------------------------------

data Delayed m env req a where
  Delayed :: { delayedCaptures :: env -> DelayedM m req captures
             , delayedBody :: DelayedM m req body
             , delayedParams :: DelayedM m req params
             , delayedHandler :: captures -> body -> params -> req -> RouteResult a
             } -> Delayed m env req a

instance Functor m => Functor (Delayed m env req) where
  fmap f Delayed{..} =
    Delayed { delayedHandler = \captures body params req -> f <$> delayedHandler captures body params req
            , ..
            }

runDelayed
  :: Monad m
  => Delayed m env req a
  -> env
  -> req
  -> m (RouteResult a)
runDelayed Delayed{..} env = runDelayedM (do
    req <- ask
    captures <- delayedCaptures env
    body <- delayedBody
    params <- delayedParams
    liftRouteResult $ delayedHandler captures body params req
  )

runAction
  :: Delayed (Sem r) env req (Sem r a)
  -> env
  -> req
  -> (a -> Sem r (RouteResult b))
  -> Sem r (RouteResult b)
runAction action env req k = do
    res <- runDelayed action env req
    case res of
      Route a     -> k =<< a
      Fail e      -> pure $ Fail e
      FailFatal e -> pure $ FailFatal e

-- | Fail with the option to recover.
delayedFail :: Monad m => RouterError -> DelayedM m req a
delayedFail err = liftRouteResult $ Fail err

addBody
  :: Monad m
  => Delayed m env req (a -> b)
  -> DelayedM m req a
  -> Delayed m env req b
addBody Delayed{..} newBody =
  Delayed
    { delayedBody = (,) <$> delayedBody <*> newBody
    , delayedHandler = \caps (body, bodyNew) p req -> ($ bodyNew) <$> delayedHandler caps body p req
    , ..
    }

addCapture
  :: Monad m
  => Delayed m env req (a -> b)
  -> (captured -> DelayedM m req a)
  -> Delayed m (captured, env) req b
addCapture Delayed{..} new =
  Delayed
    { delayedCaptures = \ (txt, env) -> (,) <$> delayedCaptures env <*> new txt
    , delayedHandler   = \ (x, v) body p query -> ($ v) <$> delayedHandler x body p query
    , ..
    } -- Note [Existential Record Update]

addParameter
  :: Monad m
  => Delayed m env req (a -> b)
  -> DelayedM m req a
  -> Delayed m env req b
addParameter Delayed {..} new =
  Delayed
    { delayedParams = (,) <$> delayedParams <*> new
    , delayedHandler = \caps body (p, pNew) query -> ($ pNew) <$> delayedHandler caps body p query
    , ..
    }

emptyDelayed :: Monad m => RouteResult a -> Delayed m b req a
emptyDelayed response =
  let r = pure ()
  in Delayed (const r) r r $ \_ _ _ _ -> response

-- | Gain access to the incoming request.
withRequest
  :: Monad m
  => (req -> DelayedM m req a)
  -> DelayedM m req a
withRequest f = do
  req <- ask
  f req
