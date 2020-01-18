module Tendermint.SDK.BaseApp.Query.Router where

import           Control.Lens                         ((&), (.~))
import           Data.ByteArray.Base64String          (Base64String)
import           Data.Default.Class                   (def)
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Network.ABCI.Types.Messages.Response as Response
import           Network.HTTP.Types                   (decodePathSegments)
import           Polysemy                             (Sem)
import           Tendermint.SDK.BaseApp.Query.Delayed (Delayed, runAction)
import           Tendermint.SDK.BaseApp.Query.Types   (QueryArgs (..),
                                                       QueryError (..),
                                                       QueryRequest (..),
                                                       QueryResult (..),
                                                       Queryable (..),
                                                       RouteResult (..))

-- NOTE: most of this was vendored and repurposed from servant

data Router' env a =
    RChoice (Router' env a) (Router' env a)
  | RStatic (Map Text (Router' env a)) [env -> a]
  | RQueryArgs (Router' (QueryArgs Base64String, env) a)

type RoutingApplication r = QueryRequest -> Sem r (RouteResult Response.Query)

type Router env r = Router' env (RoutingApplication r)

pathRouter :: Text -> Router' env a -> Router' env a
pathRouter t r = RStatic (M.singleton t r) []

leafRouter :: (env -> a) -> Router' env a
leafRouter l = RStatic M.empty [l]

choice :: Router' env a -> Router' env a -> Router' env a
choice (RStatic table1 ls1) (RStatic table2 ls2) =
  RStatic (M.unionWith choice table1 table2) (ls1 ++ ls2)
choice router1 (RChoice router2 router3) = RChoice (choice router1 router2) router3
choice router1 router2 = RChoice router1 router2


methodRouter
  :: Queryable b
  => Delayed (Sem r) env (Sem r (QueryResult b))
  -> Router env r
methodRouter action = leafRouter route'
  where
    route' env query = runAction action env query $ \QueryResult{..} ->
       Route $ def & Response._queryIndex .~ queryResultIndex
                   & Response._queryKey .~ queryResultKey
                   & Response._queryValue .~ encodeQueryResult queryResultData
                   & Response._queryProof .~ queryResultProof
                   & Response._queryHeight .~ queryResultHeight

runRouter
  :: Router env r
  -> env
  -> RoutingApplication r
runRouter router env query =
  case router of
    RStatic table ls ->
      let path = decodePathSegments . T.encodeUtf8 $ queryRequestPath query
      in case path of
        []   -> runChoice ls env query
        -- This case is to handle trailing slashes.
        [""] -> runChoice ls env query
        first : rest | Just router' <- M.lookup first table
          -> let query' = query { queryRequestPath = T.intercalate "/" rest }
             in  runRouter router' env query'
        _ -> pure $ Fail PathNotFound
    RQueryArgs r' ->
      let qa = QueryArgs
            { queryArgsData = queryRequestData query
            , queryArgsHeight = queryRequestHeight query
            , queryArgsProve = queryRequestProve query
            }
      in runRouter r' (qa, env) query
    RChoice r1 r2 ->
      runChoice [runRouter r1, runRouter r2] env query

runChoice :: [env -> RoutingApplication r] -> env -> RoutingApplication r
runChoice ls =
  case ls of
    []       -> \ _ _ -> pure $ Fail PathNotFound
    [r]      -> r
    (r : rs) ->
      \ env query -> do
        response1 <- r env query
        case response1 of
          Fail _ -> runChoice rs env query
          _      ->  pure response1
