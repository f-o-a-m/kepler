{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Query.Router
  ( HasQueryRouter(..)
  , methodRouter
  ) where

import           Control.Monad                        (join)
import           Data.Proxy
import           Data.String.Conversions              (cs)
import           Data.Text                            (Text)
import           GHC.TypeLits                         (KnownSymbol, symbolVal)
import           Network.ABCI.Types.Messages.Response as Response
import           Network.HTTP.Types.URI               (QueryText,
                                                       parseQueryText)
import           Polysemy                             (Member, Sem)
import           Servant.API
import           Servant.API.Modifiers                (FoldLenient,
                                                       FoldRequired,
                                                       RequestArgument,
                                                       unfoldRequestArgument)
import           Tendermint.SDK.BaseApp.Query.Effect  (QueryEffs, runQuery)
import           Tendermint.SDK.BaseApp.Query.Types   (EmptyQueryServer (..),
                                                       FromQueryData (..), Leaf,
                                                       QA, QueryArgs (..),
                                                       QueryRequest (..),
                                                       QueryResult (..))
import qualified Tendermint.SDK.BaseApp.Router        as R
import           Tendermint.SDK.BaseApp.Store         (ReadStore)
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Tendermint.SDK.Types.Effects         ((:&))
import           Web.HttpApiData                      (FromHttpApiData (..),
                                                       parseUrlPieceMaybe)


--------------------------------------------------------------------------------

-- | This class is used to construct a router given a 'layout' type. The layout
-- | is constructed using the combinators that appear in the instances here, no other
-- | Servant combinators are recognized.
class HasQueryRouter layout r where
  -- | A routeQ handler.
  type RouteQ layout r :: *
  -- | Transform a routeQ handler into a 'Router'.
  routeQ
    :: Proxy layout
    -> Proxy r
    -> R.Delayed (Sem r) env QueryRequest (RouteQ layout (QueryEffs :& r))
    -> R.Router env r QueryRequest Response.Query

  hoistQueryRouter :: Proxy layout -> Proxy r -> (forall a. Sem s a -> Sem s' a) -> RouteQ layout s -> RouteQ layout s'

instance (HasQueryRouter a r, HasQueryRouter b r) => HasQueryRouter (a :<|> b) r where
  type RouteQ (a :<|> b) r = RouteQ a r :<|> RouteQ b r

  routeQ _ pr server =
     R.choice (routeQ (Proxy @a) pr ((\ (a :<|> _) -> a) <$> server))
              (routeQ (Proxy @b) pr ((\ (_ :<|> b) -> b) <$> server))
  hoistQueryRouter _ pr nat (a :<|> b) =
    hoistQueryRouter (Proxy @a) pr nat a :<|> hoistQueryRouter (Proxy @b) pr nat b

instance (HasQueryRouter sublayout r, KnownSymbol path) => HasQueryRouter (path :> sublayout) r where

  type RouteQ (path :> sublayout) r = RouteQ sublayout r

  routeQ _ pr subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (routeQ (Proxy :: Proxy sublayout) pr subserver)
    where proxyPath = Proxy :: Proxy path

  hoistQueryRouter _ pr nat = hoistQueryRouter (Proxy @sublayout) pr nat

instance ( HasQueryRouter sublayout r, KnownSymbol sym, FromHttpApiData a
         , SBoolI (FoldRequired mods), SBoolI (FoldLenient mods)
         ) => HasQueryRouter (QueryParam' mods sym a :> sublayout) r where

  type RouteQ (QueryParam' mods sym a :> sublayout) r = RequestArgument mods a -> RouteQ sublayout r

  routeQ _ pr subserver =
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
    in routeQ (Proxy :: Proxy sublayout) pr delayed

  hoistQueryRouter _ pr nat f = hoistQueryRouter (Proxy @sublayout) pr nat . f

instance (FromHttpApiData a, HasQueryRouter sublayout r) => HasQueryRouter (Capture' mods capture a :> sublayout) r where

  type RouteQ (Capture' mods capture a :> sublayout) r = a -> RouteQ sublayout r

  routeQ _ pr subserver =
    R.CaptureRouter $
        routeQ (Proxy :: Proxy sublayout)
              pr
              (R.addCapture subserver $ \ txt -> case parseUrlPieceMaybe txt of
                 Nothing -> R.delayedFail R.PathNotFound
                 Just v  -> return v
              )
  hoistQueryRouter _ pr nat f = hoistQueryRouter (Proxy @sublayout) pr nat . f

instance (FromQueryData a, HasQueryRouter sublayout r) => HasQueryRouter (QA a :> sublayout) r where

  type RouteQ (QA a :> sublayout) r = QueryArgs a -> RouteQ sublayout r

  routeQ _ pr subserver =
    let parseQueryArgs QueryRequest{..} = case fromQueryData queryRequestData of
          Left e -> R.delayedFail $ R.InvalidRequest ("Error parsing query data, " <> cs e <> ".")
          Right a -> pure QueryArgs
            { queryArgsData = a
            , queryArgsHeight = queryRequestHeight
            , queryArgsProve = queryRequestProve
            }
        delayed = R.addBody subserver $ R.withRequest parseQueryArgs
    in routeQ (Proxy :: Proxy sublayout) pr delayed

  hoistQueryRouter _ pr nat f = hoistQueryRouter (Proxy @sublayout) pr nat . f

instance (Member ReadStore r, HasCodec a) => HasQueryRouter (Leaf a) r where

   type RouteQ (Leaf a) r = Sem r (QueryResult a)
   routeQ _ _ = methodRouter
   hoistQueryRouter _ _ = ($)

instance HasQueryRouter EmptyQueryServer r where
  type RouteQ EmptyQueryServer r = EmptyQueryServer
  routeQ _ _ _ = R.StaticRouter mempty mempty
  hoistQueryRouter _ _ _ = id

--------------------------------------------------------------------------------

methodRouter
  :: HasCodec a
  => Member ReadStore r
  => R.Delayed (Sem r) env req (Sem (QueryEffs :& r) (QueryResult a))
  -> R.Router env r req Response.Query
methodRouter action =
  let route' env q = R.runAction (runQuery <$> action) env q (pure . R.Route)
  in R.leafRouter route'
