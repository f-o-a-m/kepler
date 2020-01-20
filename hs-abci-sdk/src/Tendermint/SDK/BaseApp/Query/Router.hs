module Tendermint.SDK.BaseApp.Query.Router where

import           Control.Lens                         ((&), (.~))
import           Data.ByteArray.Base64String          (fromBytes)
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
import           Tendermint.SDK.BaseApp.Query.Types   (QueryError (..),
                                                       QueryRequest (..),
                                                       QueryResult (..),
                                                       RouteResult (..))
import           Tendermint.SDK.Codec                 (HasCodec (..))


-- NOTE: most of this was vendored and repurposed from servant

data Router' env a =
    StaticRouter (Map Text (Router' env a)) [env -> a]
  | CaptureRouter (Router' (Text, env) a)
  | Choice (Router' env a) (Router' env a)


type RoutingApplication r = QueryRequest -> Sem r (RouteResult Response.Query)

type Router env r = Router' env (RoutingApplication r)

pathRouter :: Text -> Router' env a -> Router' env a
pathRouter t r = StaticRouter (M.singleton t r) []

leafRouter :: (env -> a) -> Router' env a
leafRouter l = StaticRouter M.empty [l]

choice :: Router' env a -> Router' env a -> Router' env a
choice (StaticRouter table1 ls1) (StaticRouter table2 ls2) =
  StaticRouter (M.unionWith choice table1 table2) (ls1 ++ ls2)
choice (CaptureRouter router1) (CaptureRouter router2) =
  CaptureRouter (choice router1 router2)
choice router1 (Choice router2 router3) = Choice (choice router1 router2) router3
choice router1 router2 = Choice router1 router2


methodRouter
  :: HasCodec b
  => Delayed (Sem r) env (Sem r (QueryResult b))
  -> Router env r
methodRouter action = leafRouter route'
  where
    route' env query = runAction action env query $ \QueryResult{..} ->
       Route $ def & Response._queryIndex .~ queryResultIndex
                   & Response._queryKey .~ queryResultKey
                   & Response._queryValue .~ fromBytes (encode queryResultData)
                   & Response._queryProof .~ queryResultProof
                   & Response._queryHeight .~ queryResultHeight

runRouter
  :: Router env r
  -> env
  -> RoutingApplication r
runRouter router env query =
  case router of
    StaticRouter table ls ->
      let path = decodePathSegments . T.encodeUtf8 $ queryRequestPath query
      in case path of
        []   -> runChoice ls env query
        -- This case is to handle trailing slashes.
        [""] -> runChoice ls env query
        first : rest | Just router' <- M.lookup first table
          -> let query' = query { queryRequestPath = T.intercalate "/" rest }
             in  runRouter router' env query'
        _ -> pure $ Fail PathNotFound
    CaptureRouter router' ->
      let path = decodePathSegments . T.encodeUtf8 $ queryRequestPath query
      in case path of
          []   -> pure $ Fail PathNotFound
          -- This case is to handle trailing slashes.
          [""] -> pure $ Fail PathNotFound
          first : rest
            -> let query' = query { queryRequestPath = T.intercalate "/" rest }
               in  runRouter router' (first, env) query'
    Choice r1 r2 ->
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
