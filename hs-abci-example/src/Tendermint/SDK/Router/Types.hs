module Tendermint.SDK.Router.Types where

import           Control.Monad                          (ap)
import           Control.Monad.Except                   (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class                 (MonadIO (..))
import           Control.Monad.Trans                    (MonadTrans (..))
import           Data.ByteArray.HexString               (HexString)
import           Data.Int                               (Int64)
import           Network.ABCI.Types.Messages.FieldTypes (Proof)
import qualified Network.ABCI.Types.Messages.Request    as Request
import qualified Network.ABCI.Types.Messages.Response   as Response


data Leaf (a :: *)

data QA (a :: *)

--------------------------------------------------------------------------------

type Application m  = Request.Query -> m Response.Query

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
  , queryArgsQueryData   :: HexString
  , queryArgsBlockHeight :: Int64
  } deriving Functor

data QueryResult a = QueryResult
  { queryResultData   :: a
  , queryResultIndex  :: Int64
  , queryResultKey    :: HexString
  , queryResultProof  :: Maybe Proof
  , queryResultHeight :: Int64
  } deriving Functor

--------------------------------------------------------------------------------

class EncodeQueryResult a where
  encodeQueryResult :: a -> HexString

class FromQueryData a where
  fromQueryData :: HexString -> Either String a

--------------------------------------------------------------------------------

newtype HandlerT m a =
  HandlerT { _runHandlerT :: ExceptT QueryError m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadError QueryError)

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
    Route a     -> f a
    Fail e      -> Fail e
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
      Route a'    -> runRouteResultT $ f a'
      Fail e      -> return $ Fail e
      FailFatal e -> return $ FailFatal e

instance MonadIO m => MonadIO (RouteResultT m) where
  liftIO = lift . liftIO
