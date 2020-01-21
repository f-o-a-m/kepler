{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Query.Class where

import           Control.Lens                         ((&), (.~))
import           Control.Monad                        (join)
import           Data.ByteArray.Base64String          (fromBytes)
import           Data.Default.Class                   (def)
import           Data.Proxy
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import           GHC.TypeLits                         (KnownSymbol, symbolVal)
import           Network.ABCI.Types.Messages.Response as Response
import           Network.HTTP.Types.URI               (QueryText,
                                                       parseQueryText)
import           Polysemy                             (Sem)
import           Servant.API
import           Servant.API.Modifiers                (FoldLenient,
                                                       FoldRequired,
                                                       RequestArgument,
                                                       unfoldRequestArgument)
import           Tendermint.SDK.BaseApp.Query.Types   (FromQueryData (..), Leaf,
                                                       QA, QueryArgs (..),
                                                       QueryRequest (..),
                                                       QueryResult (..))
import qualified Tendermint.SDK.BaseApp.Router        as R
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Web.HttpApiData                      (FromHttpApiData (..),
                                                       parseUrlPieceMaybe)


--------------------------------------------------------------------------------

-- | This class is used to construct a router given a 'layout' type. The layout
-- | is constructed using the combinators that appear in the instances here, no other
-- | Servant combinators are recognized.
class HasRouter layout r where
  -- | A route handler.
  type RouteT layout r :: *
  -- | Transform a route handler into a 'Router'.
  route :: Proxy layout -> Proxy r -> R.Delayed (Sem r) env QueryRequest (RouteT layout r)
        -> R.Router env r QueryRequest Response.Query

instance (HasRouter a r, HasRouter b r) => HasRouter (a :<|> b) r where
  type RouteT (a :<|> b) r = RouteT a r :<|> RouteT b r

  route _ pr server = R.choice (route pa pr ((\ (a :<|> _) -> a) <$> server))
                        (route pb pr ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance (HasRouter sublayout r, KnownSymbol path) => HasRouter (path :> sublayout) r where

  type RouteT (path :> sublayout) r = RouteT sublayout r

  route _ pr subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (route (Proxy :: Proxy sublayout) pr subserver)
    where proxyPath = Proxy :: Proxy path

instance ( HasRouter sublayout r, KnownSymbol sym, FromHttpApiData a
         , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
         ) => HasRouter (QueryParam' mods sym a :> sublayout) r where

  type RouteT (QueryParam' mods sym a :> sublayout) r = RequestArgument mods a -> RouteT sublayout r

  route _ pr subserver =
    let querytext :: QueryRequest -> Network.HTTP.Types.URI.QueryText
        querytext q = parseQueryText . cs $ queryRequestParamString q
        paramname = cs $ symbolVal (Proxy :: Proxy sym)
        parseParam q = unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
          where
            mev :: Maybe (Either Text a)
            mev = fmap parseQueryParam $ join $ lookup paramname $ querytext q
            errReq = R.delayedFail $ R.InvalidRequest ("Query parameter " <> cs paramname <> " is required.")
            errSt e = R.delayedFail $ R.InvalidRequest ("Error parsing query param " <> cs paramname <> " " <> cs e <> ".")
        delayed = R.addParameter subserver $ R.withRequest parseParam
    in route (Proxy :: Proxy sublayout) pr delayed

instance (FromHttpApiData a, HasRouter sublayout r) => HasRouter (Capture' mods capture a :> sublayout) r where

  type RouteT (Capture' mods capture a :> sublayout) r = a -> RouteT sublayout r

  route _ pr subserver =
    R.CaptureRouter $
        route (Proxy :: Proxy sublayout)
              pr
              (R.addCapture subserver $ \ txt -> case parseUrlPieceMaybe txt of
                 Nothing -> R.delayedFail R.PathNotFound
                 Just v  -> return v
              )

instance HasCodec a => HasRouter (Leaf a) r where

   type RouteT (Leaf a) r = Sem r (QueryResult a)
   route _ _ = methodRouter

instance (FromQueryData a, HasRouter sublayout r)
      => HasRouter (QA a :> sublayout) r where

  type RouteT (QA a :> sublayout) r = QueryArgs a -> RouteT sublayout r

  route _ pr subserver =
    let parseQueryArgs QueryRequest{..} = case fromQueryData queryRequestData of
          Left e -> R.delayedFail $ R.InvalidRequest ("Error parsing query data, " <> cs e <> ".")
          Right a -> pure QueryArgs
            { queryArgsData = a
            , queryArgsHeight = queryRequestHeight
            , queryArgsProve = queryRequestProve
            }
        delayed = R.addBody subserver $ R.withRequest parseQueryArgs
    in route (Proxy :: Proxy sublayout) pr delayed

--------------------------------------------------------------------------------

methodRouter
  :: HasCodec b
  => R.Delayed (Sem r) env req (Sem r (QueryResult b))
  -> R.Router env r req Response.Query
methodRouter action = R.leafRouter route'
  where
    route' env query = R.runAction action env query $ \QueryResult{..} ->
       R.Route $ def & Response._queryIndex .~ queryResultIndex
                   & Response._queryKey .~ queryResultKey
                   & Response._queryValue .~ fromBytes (encode queryResultData)
                   & Response._queryProof .~ queryResultProof
                   & Response._queryHeight .~ queryResultHeight
