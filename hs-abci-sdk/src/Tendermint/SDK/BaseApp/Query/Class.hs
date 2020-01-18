{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Query.Class where

import           Control.Monad                        (join)
import           Data.Proxy
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import           GHC.TypeLits                         (KnownSymbol, symbolVal)
import           Network.HTTP.Types.URI               (QueryText,
                                                       parseQueryText)
import           Polysemy                             (Sem)
import           Servant.API
import           Servant.API.Modifiers                (FoldLenient,
                                                       FoldRequired,
                                                       RequestArgument,
                                                       unfoldRequestArgument)
import           Tendermint.SDK.BaseApp.Query.Delayed (Delayed, DelayedM,
                                                       addParameter,
                                                       addQueryArgs,
                                                       delayedFail, withQuery)
import           Tendermint.SDK.BaseApp.Query.Router  (Router, Router' (..),
                                                       choice, methodRouter,
                                                       pathRouter)
import           Tendermint.SDK.BaseApp.Query.Types   (FromQueryData (..), Leaf,
                                                       QA, QueryArgs (..),
                                                       QueryError (..),
                                                       QueryRequest (..),
                                                       QueryResult)
import           Tendermint.SDK.Codec                 (HasCodec)
import           Web.Internal.HttpApiData             (FromHttpApiData (..))

--------------------------------------------------------------------------------

-- | This class is used to construct a router given a 'layout' type. The layout
-- | is constructed using the combinators that appear in the instances here, no other
-- | Servant combinators are recognized.
class HasRouter layout r where
  -- | A route handler.
  type RouteT layout r :: *
  -- | Transform a route handler into a 'Router'.
  route :: Proxy layout -> Proxy r -> Delayed (Sem r) env (RouteT layout r) -> Router env r

instance (HasRouter a r, HasRouter b r) => HasRouter (a :<|> b) r where
  type RouteT (a :<|> b) r = RouteT a r :<|> RouteT b r

  route _ pr server = choice (route pa pr ((\ (a :<|> _) -> a) <$> server))
                        (route pb pr ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance (HasRouter sublayout r, KnownSymbol path) => HasRouter (path :> sublayout) r where

  type RouteT (path :> sublayout) r = RouteT sublayout r

  route _ pr subserver =
    pathRouter (cs (symbolVal proxyPath)) (route (Proxy :: Proxy sublayout) pr subserver)
    where proxyPath = Proxy :: Proxy path

instance ( HasRouter sublayout r, KnownSymbol sym, FromHttpApiData a
         , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
         ) => HasRouter (QueryParam' mods sym a :> sublayout) r where

  type RouteT (QueryParam' mods sym a :> sublayout) r = RequestArgument mods a -> RouteT sublayout r

  route _ pr subserver =
    let querytext :: QueryRequest -> Network.HTTP.Types.URI.QueryText
        querytext q = parseQueryText . cs $ queryRequestParamString q
        paramname = cs $ symbolVal (Proxy :: Proxy sym)
        parseParam :: Monad m => QueryRequest -> DelayedM m (RequestArgument mods a)
        parseParam q = unfoldRequestArgument (Proxy :: Proxy mods) errReq errSt mev
          where
            mev :: Maybe (Either Text a)
            mev = fmap parseQueryParam $ join $ lookup paramname $ querytext q
            errReq = delayedFail $ InvalidQuery ("Query parameter " <> cs paramname <> " is required.")
            errSt e = delayedFail $ InvalidQuery ("Error parsing query param " <> cs paramname <> " " <> cs e <> ".")
        delayed = addParameter subserver $ withQuery parseParam
    in route (Proxy :: Proxy sublayout) pr delayed

instance HasCodec a => HasRouter (Leaf a) r where

   type RouteT (Leaf a) r = Sem r (QueryResult a)
   route _ _ = methodRouter

instance (FromQueryData a, HasRouter sublayout r)
      => HasRouter (QA a :> sublayout) r where

  type RouteT (QA a :> sublayout) r = QueryArgs a -> RouteT sublayout r

  route _ pr d =
    RQueryArgs $
      route (Proxy :: Proxy sublayout) pr
          (addQueryArgs d $ \ qa -> case fromQueryData $ queryArgsData qa of
             Left e  -> delayedFail $ InvalidQuery (cs e)
             Right v -> return qa {queryArgsData = v}
          )
