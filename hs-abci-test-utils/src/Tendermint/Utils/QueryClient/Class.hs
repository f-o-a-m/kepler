{-# LANGUAGE UndecidableInstances #-}
module Tendermint.Utils.QueryClient.Class where

import           Control.Lens                           ((^.))
import           Control.Monad.Reader                   (ReaderT)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteArray.HexString               as Hex
import           Data.ByteString                        (ByteString)
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text, intercalate)
import           Data.Word                              (Word64)
import           GHC.TypeLits                           (KnownSymbol, symbolVal)
import           Network.ABCI.Types.Messages.FieldTypes (WrappedVal (..))
import qualified Network.ABCI.Types.Messages.Request    as Req
import qualified Network.ABCI.Types.Messages.Response   as Resp
import qualified Network.Tendermint.Client              as RPC
import           Servant.API
import           Servant.API.Modifiers
import           Tendermint.SDK.BaseApp.Errors          (queryAppError)
import           Tendermint.SDK.BaseApp.Query.Store     (StoreLeaf)
import           Tendermint.SDK.BaseApp.Query.Types     (Leaf, QA,
                                                         QueryArgs (..),
                                                         QueryData (..),
                                                         QueryResult (..))
import qualified Tendermint.SDK.BaseApp.Store.Array     as A
import qualified Tendermint.SDK.BaseApp.Store.Map       as M
import qualified Tendermint.SDK.BaseApp.Store.Var       as V
import           Tendermint.SDK.Codec                   (HasCodec (decode))
import           Tendermint.Utils.QueryClient.Types
import           Web.Internal.HttpApiData               (ToHttpApiData (..))

class Monad m => RunQueryClient m where
    -- | How to make a request.
    runQuery :: Req.Query -> m Resp.Query

instance RunQueryClient (ReaderT RPC.Config IO) where
  runQuery Req.Query{..} =
    let rpcQ = RPC.RequestABCIQuery
          { RPC.requestABCIQueryPath = Just queryPath
          , RPC.requestABCIQueryData = Hex.fromBytes @ByteString . Base64.toBytes $ queryData
          , RPC.requestABCIQueryHeight = Just $ queryHeight
          , RPC.requestABCIQueryProve  = queryProve
          }
    in RPC.resultABCIQueryResponse <$> RPC.abciQuery rpcQ

type QueryStringList = [(Text, Text)]

class HasQueryClient m layout where

    type ClientQ (m :: * -> *) layout :: *
    genClientQ :: Proxy m -> Proxy layout -> (Req.Query, QueryStringList) -> ClientQ m layout

instance (HasQueryClient m a, HasQueryClient m b) => HasQueryClient m (a :<|> b) where
    type ClientQ m (a :<|> b) = ClientQ m a :<|> ClientQ m b
    genClientQ pm _ (q,qs) = genClientQ pm (Proxy @a) (q,qs) :<|> genClientQ pm (Proxy @b) (q,qs)

instance (KnownSymbol path, HasQueryClient m a) => HasQueryClient m (path :> a) where
    type ClientQ m (path :> a) = ClientQ m a
    genClientQ pm _ (q,qs) = genClientQ pm (Proxy @a)
      (q {Req.queryPath = Req.queryPath q <> "/" <> cs (symbolVal (Proxy @path))}, qs)

appendToQueryString
  :: Text       -- ^ param name
  -> Maybe Text -- ^ param value
  -> QueryStringList
  -> QueryStringList
appendToQueryString pname pvalue qs =
  maybe qs (\v -> (pname, v) : qs) pvalue

instance (KnownSymbol sym, ToHttpApiData a, HasQueryClient m api, SBoolI (FoldRequired mods))
      => HasQueryClient m (QueryParam' mods sym a :> api) where

  type ClientQ m (QueryParam' mods sym a :> api) = RequiredArgument mods a -> ClientQ m api

  -- if mparam = Nothing, we don't add it to the query string
  genClientQ pm Proxy (q,qs) mparam =
    genClientQ pm (Proxy :: Proxy api) $ foldRequiredArgument
      (Proxy :: Proxy mods) add (maybe (q,qs) add) mparam
    where
      add :: a -> (Req.Query, QueryStringList)
      add param = (q, appendToQueryString pname (Just $ toQueryParam param) qs)

      pname :: Text
      pname  = cs $ symbolVal (Proxy :: Proxy sym)

instance (QueryData k, HasQueryClient m a) => HasQueryClient m (QA k :> a) where
    type ClientQ m (QA k :> a) = QueryArgs k -> ClientQ m a
    genClientQ pm _ (q,qs) QueryArgs{..} = genClientQ pm (Proxy @a)
      (q { Req.queryData = toQueryData queryArgsData
         , Req.queryHeight = WrappedVal queryArgsHeight
         , Req.queryProve = queryArgsProve
         }, qs)

instance (ToHttpApiData a, HasQueryClient m api) => HasQueryClient m (Capture' mods capture a :> api) where

  type ClientQ m (Capture' mods capture a :> api) = a -> ClientQ m api

  genClientQ pm _ (q,qs) val =
    let p = toUrlPiece val
        q' = q { Req.queryPath = Req.queryPath q <> "/" <> p }
    in genClientQ pm (Proxy :: Proxy api) (q', qs)

addQueryParamsToPath
  :: QueryStringList
  -> Text
  -> Text
addQueryParamsToPath qs path =
  let qParams = intercalate "&" $ map (\(n,v) -> n <> "=" <> v) qs
  in case qs of
       [] -> path
       _  -> path <> "?" <> qParams

instance (HasCodec a, RunQueryClient m) => HasQueryClient m (Leaf a) where
    type ClientQ m (Leaf a) = m (QueryClientResponse a)
    genClientQ _ _ = leafGenClient

leafGenClient
  :: HasCodec a
  => RunQueryClient m
  => (Req.Query, QueryStringList)
  -> m (QueryClientResponse a)
leafGenClient (q,qs) = do
  let reqPath = addQueryParamsToPath qs $ Req.queryPath q
  r@Resp.Query{..} <- runQuery q { Req.queryPath = reqPath }
  -- anything other than 0 code is a failure: https://tendermint.readthedocs.io/en/latest/abci-spec.html
  -- and will result in queryValue decoding to a "empty/default" object
  return $ case queryCode of
    0 -> case decode $ Base64.toBytes queryValue of
           Left err -> error $ "Impossible parse error: " <> cs err
           Right a  -> QueryResponse $ QueryResult
             { queryResultData = a
             , queryResultIndex = unWrappedVal queryIndex
             , queryResultHeight =  unWrappedVal queryHeight
             , queryResultProof = queryProof
             , queryResultKey = queryKey
             }
    _ -> QueryError $ r ^. queryAppError

instance (HasCodec a, RunQueryClient m) => HasQueryClient m (StoreLeaf (V.Var a)) where
    type ClientQ m (StoreLeaf (V.Var a)) = ClientQ m (QA () :> Leaf a)
    genClientQ pm _ = genClientQ pm (Proxy @(QA () :> Leaf a))

instance (HasCodec a, RunQueryClient m) => HasQueryClient m (StoreLeaf (A.Array a)) where
    type ClientQ m (StoreLeaf (A.Array a)) = ClientQ m (QA Word64 :> Leaf a)
    genClientQ pm _ = genClientQ pm (Proxy @(QA Word64 :> Leaf a))

instance (QueryData k, HasCodec v, RunQueryClient m) => HasQueryClient m (StoreLeaf (M.Map k v)) where
    type ClientQ m (StoreLeaf (M.Map k v)) = ClientQ m (QA k :> Leaf v)
    genClientQ pm _ = genClientQ pm (Proxy @(QA k :> Leaf v))

-- | Singleton type representing a client for an empty API.
data EmptyQueryClient = EmptyQueryClient deriving (Eq, Show, Bounded, Enum)

instance HasQueryClient m EmptyQueryClient where
  type ClientQ m EmptyQueryClient = EmptyQueryClient

  genClientQ _ _ _ = EmptyQueryClient
