module Tendermint.SDK.BaseApp.Router.Types
  ( Application
  , RouterError(..)
  , RouteResult(..)
  , RouteResultT(..)
  , HasPath(..)
  ) where

import           Control.Lens                  (Lens')
import           Control.Monad                 (ap)
import           Control.Monad.Trans           (MonadTrans (..))
import           Data.Text                     (Text)
import           Tendermint.SDK.BaseApp.Errors (AppError (..), IsAppError (..))

--------------------------------------------------------------------------------

type Application m req res = req -> m (RouteResult res)

--------------------------------------------------------------------------------

data RouterError =
    PathNotFound
  | ResourceNotFound
  | InvalidRequest Text
  | InternalError Text
  deriving (Show)

instance IsAppError RouterError where
  makeAppError PathNotFound =
    AppError
      { appErrorCode = 1
      , appErrorCodespace = "router"
      , appErrorMessage = "Path not found."
      }
  makeAppError ResourceNotFound =
    AppError
      { appErrorCode = 2
      , appErrorCodespace = "router"
      , appErrorMessage = "Resource not found."
      }
  makeAppError (InvalidRequest msg) =
    AppError
      { appErrorCode = 3
      , appErrorCodespace = "router"
      , appErrorMessage = "Invalid request: " <> msg
      }
  makeAppError (InternalError _) =
    AppError
      { appErrorCode = 4
      , appErrorCodespace = "router"
      , appErrorMessage = "Internal error."
      }

--------------------------------------------------------------------------------
-- NOTE: most of this was vendored and repurposed from servant.

data RouteResult a =
    Fail RouterError
  | FailFatal RouterError
  | Route a
  deriving (Functor)

instance Applicative RouteResult where
  pure  = return
  (<*>) = ap

instance Monad RouteResult where
  return = Route
  (>>=) m f = case m of
    Route     a -> f a
    Fail      e -> Fail e
    FailFatal e -> FailFatal e

data RouteResultT m a = RouteResultT { runRouteResultT :: m (RouteResult a) }
  deriving (Functor)

instance MonadTrans RouteResultT where
  lift m = RouteResultT $ fmap Route m

instance Monad m => Applicative (RouteResultT m) where
  pure  = return
  (<*>) = ap

instance Monad m => Monad (RouteResultT m) where
  return = RouteResultT . return . Route
  (>>=) m f = RouteResultT $ do
    a <- runRouteResultT m
    case a of
      Route     a' -> runRouteResultT $ f a'
      Fail      e  -> return $ Fail e
      FailFatal e  -> return $ FailFatal e

class HasPath t where
  path :: Lens' t Text
