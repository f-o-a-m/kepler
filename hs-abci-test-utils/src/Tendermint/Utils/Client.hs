{-# LANGUAGE UndecidableInstances #-}

module Tendermint.Utils.Client
  ( RunClient(..)
  , HasClient(..)
  , ClientResponse(..)
  ) where

import           Control.Lens                           (to, (^.))
import           Control.Monad.Reader                   (ReaderT)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteArray.HexString               as Hex
import           Data.ByteString                        (ByteString)
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text, intercalate)
import           GHC.TypeLits                           (KnownSymbol, symbolVal)
import           Network.ABCI.Types.Messages.FieldTypes (WrappedVal (..))
import qualified Network.ABCI.Types.Messages.Request    as Req
import qualified Network.ABCI.Types.Messages.Response   as Resp
import qualified Network.Tendermint.Client              as RPC
import           Servant.API
import           Servant.API.Modifiers
import           Tendermint.SDK.BaseApp.Query.Store     (StoreLeaf)
import           Tendermint.SDK.BaseApp.Query.Types     (Leaf, QA,
                                                         QueryArgs (..),
                                                         Queryable (..))
import           Tendermint.SDK.BaseApp.Store           (RawKey (..))
import           Tendermint.SDK.Codec                   (HasCodec (decode))
import           Web.Internal.HttpApiData               (ToHttpApiData (..))

class Monad m => RunClient m where
    -- | How to make a request.
    runQuery :: Req.Query -> m Resp.Query

instance RunClient (ReaderT RPC.Config IO) where
  runQuery Req.Query{..} =
    let rpcQ = RPC.RequestABCIQuery
          { RPC.requestABCIQueryPath = Just queryPath
          , RPC.requestABCIQueryData = Hex.fromBytes @ByteString . Base64.toBytes $ queryData
          , RPC.requestABCIQueryHeight = Just $ queryHeight
          , RPC.requestABCIQueryProve  = queryProve
          }
    in RPC.resultABCIQueryResponse <$> RPC.abciQuery rpcQ

type QueryStringList = [(Text, Text)]

class HasClient m layout where

    type ClientT (m :: * -> *) layout :: *
    genClient :: Proxy m -> Proxy layout -> (Req.Query, QueryStringList) -> ClientT m layout

instance (HasClient m a, HasClient m b) => HasClient m (a :<|> b) where
    type ClientT m (a :<|> b) = ClientT m a :<|> ClientT m b
    genClient pm _ (q,qs) = genClient pm (Proxy @a) (q,qs) :<|> genClient pm (Proxy @b) (q,qs)

instance (KnownSymbol path, HasClient m a) => HasClient m (path :> a) where
    type ClientT m (path :> a) = ClientT m a
    genClient pm _ (q,qs) = genClient pm (Proxy @a)
      (q {Req.queryPath = Req.queryPath q <> "/" <> cs (symbolVal (Proxy @path))}, qs)

appendToQueryString
  :: Text       -- ^ param name
  -> Maybe Text -- ^ param value
  -> QueryStringList
  -> QueryStringList
appendToQueryString pname pvalue qs =
  maybe qs (\v -> (pname, v) : qs) pvalue

instance (KnownSymbol sym, ToHttpApiData a, HasClient m api, SBoolI (FoldRequired mods))
      => HasClient m (QueryParam' mods sym a :> api) where

  type ClientT m (QueryParam' mods sym a :> api) = RequiredArgument mods a -> ClientT m api

  -- if mparam = Nothing, we don't add it to the query string
  genClient pm Proxy (q,qs) mparam =
    genClient pm (Proxy :: Proxy api) $ foldRequiredArgument
      (Proxy :: Proxy mods) add (maybe (q,qs) add) mparam
    where
      add :: a -> (Req.Query, QueryStringList)
      add param = (q, appendToQueryString pname (Just $ toQueryParam param) qs)

      pname :: Text
      pname  = cs $ symbolVal (Proxy :: Proxy sym)

instance (RawKey k, HasClient m a) => HasClient m (QA k :> a) where
    type ClientT m (QA k :> a) = QueryArgs k -> ClientT m a
    genClient pm _ (q,qs) QueryArgs{..} = genClient pm (Proxy @a)
      (q { Req.queryData = queryArgsData ^. rawKey . to Base64.fromBytes
         , Req.queryHeight = WrappedVal queryArgsHeight
         , Req.queryProve = queryArgsProve
         }, qs)

instance (ToHttpApiData a, HasClient m api) => HasClient m (Capture' mods capture a :> api) where

  type ClientT m (Capture' mods capture a :> api) = a -> ClientT m api

  genClient pm _ (q,qs) val =
    let p = toUrlPiece val
        q' = q { Req.queryPath = Req.queryPath q <> "/" <> p }
    in genClient pm (Proxy :: Proxy api) (q', qs)

-- | Data is Nothing iff Raw includes a non-0 response value
data ClientResponse a = ClientResponse
  { clientResponseData :: Maybe a
  , clientResponseRaw  :: Resp.Query
  }

addQueryParamsToPath
  :: QueryStringList
  -> Text
  -> Text
addQueryParamsToPath qs path =
  let qParams = intercalate "&" $ map (\(n,v) -> n <> "=" <> v) qs
  in case qs of
       [] -> path
       _  -> path <> "?" <> qParams

instance (HasCodec a, RunClient m) => HasClient m (Leaf a) where
    type ClientT m (Leaf a) = m (ClientResponse a)
    genClient _ _ = leafGenClient

leafGenClient
  :: HasCodec a
  => RunClient m
  => (Req.Query, QueryStringList)
  -> m (ClientResponse a)
leafGenClient (q,qs) = do
  let reqPath = addQueryParamsToPath qs $ Req.queryPath q
  r@Resp.Query{..} <- runQuery q { Req.queryPath = reqPath }
  -- anything other than 0 code is a failure: https://tendermint.readthedocs.io/en/latest/abci-spec.html
  -- and will result in queryValue decoding to a "empty/default" object
  return $ case queryCode of
    0 -> case decode $ Base64.toBytes queryValue of
           Left err -> error $ "Impossible parse error: " <> cs err
           Right a  -> ClientResponse (Just a) r
    _ -> ClientResponse Nothing r

instance (RunClient m, Queryable a, name ~  Name a, KnownSymbol name ) => HasClient m (StoreLeaf a) where
    type ClientT m (StoreLeaf a) = m (ClientResponse a)
    genClient _ _ (q,qs) =
        let leaf = symbolVal (Proxy @(Name a))
            q' = q { Req.queryPath = Req.queryPath q <> "/" <> cs leaf }
        in leafGenClient (q', qs)
