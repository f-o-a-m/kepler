module Tendermint.SDK.BaseApp.Query.Router where

import           Control.Lens                       ((&), (.~), (^.))
import           Data.Map                           (Map)
import qualified Data.Map                           as M
import           Data.Text                          (Text)
import qualified Data.Text                          as T
import qualified Data.Text.Encoding                 as T
import           Network.HTTP.Types                 (decodePathSegments)
import           Polysemy                           (Sem)
import           Tendermint.SDK.BaseApp.Query.Types (HasPath (..),
                                                     QueryError (..),
                                                     RouteResult (..))


-- NOTE: most of this was vendored and repurposed from servant

data Router' env a =
    StaticRouter (Map Text (Router' env a)) [env -> a]
  | CaptureRouter (Router' (Text, env) a)
  | Choice (Router' env a) (Router' env a)


type RoutingApplication r req res = req -> Sem r (RouteResult res)

type Router env r req res = Router' env (RoutingApplication r req res)

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



runRouter
  :: HasPath req
  => Router env r req res
  -> env
  -> RoutingApplication r req res
runRouter router env req =
  case router of
    StaticRouter table ls ->
      case decodePathSegments . T.encodeUtf8 $ req ^. path  of
        []   -> runChoice ls env req
        -- This case is to handle trailing slashes.
        [""] -> runChoice ls env req
        first : rest | Just router' <- M.lookup first table
          -> let req' = req & path .~ T.intercalate "/" rest
             in  runRouter router' env req'
        _ -> pure $ Fail PathNotFound
    CaptureRouter router' ->
      case decodePathSegments . T.encodeUtf8 $ req ^. path of
        []   -> pure $ Fail PathNotFound
        -- This case is to handle trailing slashes.
        [""] -> pure $ Fail PathNotFound
        first : rest
          -> let req' = req & path .~ T.intercalate "/" rest
             in  runRouter router' (first, env) req'
    Choice r1 r2 ->
      runChoice [runRouter r1, runRouter r2] env req

runChoice :: [env -> RoutingApplication r req res] -> env -> RoutingApplication r req res
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
