module Tendermint.SDK.Router.Types where

import           Control.Monad                          (ap)
import           Control.Monad.Trans                    (MonadTrans (..))
import           Data.ByteArray.Base64String            (Base64String)
import           Network.ABCI.Types.Messages.FieldTypes (Proof,
                                                         WrappedInt64 (..))
import qualified Network.ABCI.Types.Messages.Request    as Request
import qualified Network.ABCI.Types.Messages.Response   as Response


data Leaf (a :: *)

data QA (a :: *)

--------------------------------------------------------------------------------

type Application m = Request.Query -> m Response.Query

--------------------------------------------------------------------------------

data QueryError =
    PathNotFound
  | ResourceNotFound
  | InvalidQuery String
  | InternalError String
  deriving (Show)

data QueryArgs a = QueryArgs
  { queryArgsProve       :: Bool
  , queryArgsData        :: a
  , queryArgsQueryData   :: Base64String
  , queryArgsBlockHeight :: WrappedInt64
  } deriving Functor

data QueryResult a = QueryResult
  { queryResultData   :: a
  , queryResultIndex  :: WrappedInt64
  , queryResultKey    :: Base64String
  , queryResultProof  :: Maybe Proof
  , queryResultHeight :: WrappedInt64
  } deriving Functor

--------------------------------------------------------------------------------

class EncodeQueryResult a where
  encodeQueryResult :: a -> Base64String

class FromQueryData a where
  fromQueryData :: Base64String -> Either String a

--------------------------------------------------------------------------------

data RouteResult a =
    Fail QueryError
  | FailFatal QueryError
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
