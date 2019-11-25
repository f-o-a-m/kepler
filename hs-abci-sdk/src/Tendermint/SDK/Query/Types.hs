module Tendermint.SDK.Query.Types where

import           Control.Lens                           (from, (^.))
import           Control.Monad                          (ap)
import           Control.Monad.Trans                    (MonadTrans (..))
import           Data.ByteArray.Base64String            (Base64String,
                                                         fromBytes, toBytes)
import           Data.Int                               (Int64)
import           Data.Text                              (Text)
import           GHC.TypeLits                           (Symbol)
import           Network.ABCI.Types.Messages.FieldTypes (Proof, WrappedVal (..))
import qualified Network.ABCI.Types.Messages.Request    as Request
import qualified Network.ABCI.Types.Messages.Response   as Response
import           Tendermint.SDK.Codec                   (HasCodec (..))
import           Tendermint.SDK.Store                   (RawKey (..))

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
  { queryArgsProve  :: Bool
  , queryArgsData   :: a
  , queryArgsHeight :: WrappedVal Int64
  } deriving Functor

-- wrap data with default query fields
defaultQueryWithData :: a -> QueryArgs a
defaultQueryWithData x = QueryArgs
  { queryArgsData = x
  , queryArgsHeight = 0
  , queryArgsProve = False
  }

data QueryResult a = QueryResult
  { queryResultData   :: a
  , queryResultIndex  :: WrappedVal Int64
  , queryResultKey    :: Base64String
  , queryResultProof  :: Maybe Proof
  , queryResultHeight :: WrappedVal Int64
  } deriving Functor

--------------------------------------------------------------------------------

-- | class representing objects which can be queried via the hs-abci query message.
-- | Here the 'Name' is the leaf of the query url, e.g. if you can access a token
-- | balance of type `Balance` at "token/balance", then 'Name Balance ~ "balance"'.
class Queryable a where
  type Name a :: Symbol
  encodeQueryResult :: a -> Base64String
  decodeQueryResult :: Base64String -> Either Text a

  default encodeQueryResult :: HasCodec a => a -> Base64String
  encodeQueryResult = fromBytes . encode

  default decodeQueryResult :: HasCodec a => Base64String -> Either Text a
  decodeQueryResult = decode . toBytes

-- | This class is used to parse the 'data' field of the query request message.
-- | The default method assumes that the 'data' is simply the key for the
-- | value being queried.
class FromQueryData a where
  fromQueryData :: Base64String -> Either String a

  default fromQueryData :: RawKey a => Base64String -> Either String a
  fromQueryData bs = Right (toBytes bs ^. from rawKey)

--------------------------------------------------------------------------------
-- NOTE: most of this was vendored and repurposed from servant.

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
