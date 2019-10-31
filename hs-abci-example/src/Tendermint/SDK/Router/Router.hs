module Tendermint.SDK.Router.Router where

import           Control.Error
import           Control.Lens                         (to, (&), (.~), (^.))
import           Data.ByteArray.Base64String          (Base64String)
import           Data.Default.Class                   (def)
import           Data.Map                             (Map)
import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Network.HTTP.Types                   (decodePathSegments)
import           Tendermint.SDK.Router.Delayed
import           Tendermint.SDK.Router.Types


data Router' env a =
    RChoice (Router' env a) (Router' env a)
  | RStatic (Map Text (Router' env a)) [env -> a]
  | RQueryArgs (Router' (QueryArgs Base64String, env) a)

type RoutingApplication m = Request.Query -> m (RouteResult Response.Query)

type Router env m = Router' env (RoutingApplication m)

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
  :: Monad m
  => EncodeQueryResult b
  => Delayed m env (ExceptT QueryError m (QueryResult b))
  -> Router env m
methodRouter action = leafRouter route'
  where
    route' env query = runAction action env query $ \QueryResult{..} ->
       Route $ def & Response._queryIndex .~ queryResultIndex
                   & Response._queryKey .~ queryResultKey
                   & Response._queryValue .~ encodeQueryResult queryResultData
                   & Response._queryProof .~ queryResultProof
                   & Response._queryHeight .~ queryResultHeight

runRouter
  :: Monad m
  => Router env m
  -> env
  -> RoutingApplication m
runRouter router env query =
  case router of
    RStatic table ls ->
      let path = query ^. Request._queryPath . to (decodePathSegments . T.encodeUtf8)
      in case path of
        []   -> runChoice ls env query
        -- This case is to handle trailing slashes.
        [""] -> runChoice ls env query
        first : rest | Just router' <- M.lookup first table
          -> let query' = query { Request.queryPath = T.intercalate "/" rest }
             in  runRouter router' env query'
        _ -> pure $ Fail PathNotFound
    RQueryArgs r' ->
      let qa = QueryArgs
            { queryArgsData = query ^. Request._queryData
            , queryArgsQueryData = query ^. Request._queryData
            , queryArgsBlockHeight = query ^. Request._queryHeight
            , queryArgsProve = query ^. Request._queryProve
            }
      in runRouter r' (qa, env) query
    RChoice r1 r2 ->
      runChoice [runRouter r1, runRouter r2] env query

runChoice :: Monad m => [env -> RoutingApplication m] -> env -> RoutingApplication m
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

