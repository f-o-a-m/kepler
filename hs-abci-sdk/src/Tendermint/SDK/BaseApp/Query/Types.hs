module Tendermint.SDK.BaseApp.Query.Types
  (
  -- * Server combinators
    Leaf
  , QA
  , EmptyQueryServer(..)
  , FromQueryData(..)

  -- * Query Application
  , QueryApplication
  , QueryRequest(..)
  , parseQueryRequest
  , QueryArgs(..)
  , defaultQueryArgs
  , QueryResult(..)

  ) where

import           Control.Lens                           (from, lens, (^.))
import           Data.ByteArray.Base64String            (Base64String, toBytes)
import           Data.Int                               (Int64)
import           Data.Text                              (Text, breakOn, uncons)
import           Data.Word                              (Word64)
import           Network.ABCI.Types.Messages.FieldTypes (Proof, WrappedVal (..))
import qualified Network.ABCI.Types.Messages.Request    as Request
import qualified Network.ABCI.Types.Messages.Response   as Response
import           Tendermint.SDK.BaseApp.Router.Types    (HasPath (..))
import           Tendermint.SDK.BaseApp.Store           (RawKey (..))
import           Tendermint.SDK.Types.Address           (Address)

data Leaf (a :: *)

data QA (a :: *)

--------------------------------------------------------------------------------

type QueryApplication m = Request.Query -> m Response.Query

--------------------------------------------------------------------------------

data QueryRequest = QueryRequest
  { queryRequestPath        :: Text
  , queryRequestParamString :: Text
  , queryRequestData        :: Base64String
  , queryRequestProve       :: Bool
  , queryRequestHeight      :: Int64
  } deriving (Eq, Show)

parseQueryRequest
  :: Request.Query
  -> QueryRequest
parseQueryRequest Request.Query{..} =
  let (p, queryStrQ) = breakOn "?" queryPath
      queryStr = case Data.Text.uncons queryStrQ of
        Nothing -> ""
        Just ('?', rest) -> rest
        _ -> error "Impossible result parsing query string from path."
  in QueryRequest
       { queryRequestPath = p
       , queryRequestParamString = queryStr
       , queryRequestData = queryData
       , queryRequestProve = queryProve
       , queryRequestHeight = unWrappedVal queryHeight
       }

instance HasPath QueryRequest where
  path = lens queryRequestPath (\q p -> q {queryRequestPath = p})

--------------------------------------------------------------------------------

data QueryArgs a = QueryArgs
  { queryArgsProve  :: Bool
  , queryArgsData   :: a
  , queryArgsHeight :: Int64
  } deriving Functor

-- wrap data with default query fields
defaultQueryArgs :: QueryArgs ()
defaultQueryArgs = QueryArgs
  { queryArgsData = ()
  , queryArgsHeight = -1
  , queryArgsProve = False
  }

data QueryResult a = QueryResult
  { queryResultData   :: a
  , queryResultIndex  :: Int64
  , queryResultKey    :: Base64String
  , queryResultProof  :: Maybe Proof
  , queryResultHeight :: Int64
  } deriving (Eq, Show, Functor)

--------------------------------------------------------------------------------

-- | This class is used to parse the 'data' field of the query request message.
-- | The default method assumes that the 'data' is simply the key for the
-- | value being queried.
class FromQueryData a where
  fromQueryData :: Base64String -> Either String a

  default fromQueryData :: RawKey a => Base64String -> Either String a
  fromQueryData bs = Right (toBytes bs ^. from rawKey)

instance FromQueryData Address
instance FromQueryData Word64
instance FromQueryData ()

data EmptyQueryServer = EmptyQueryServer
