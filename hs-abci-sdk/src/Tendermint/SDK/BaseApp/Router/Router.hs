module Tendermint.SDK.BaseApp.Router.Router
  ( Router
  , Router'(..)
  , runRouter
  , pathRouter
  , leafRouter
  , choice
  , makeMerge
  ) where

import           Control.Lens                        ((&), (.~), (^.))
import           Data.Map                            (Map)
import qualified Data.Map                            as M
import           Data.Text                           (Text)
import qualified Data.Text                           as T
import qualified Data.Text.Encoding                  as T
import           Network.HTTP.Types                  (decodePathSegments)
import           Polysemy                            (Sem)
import           Tendermint.SDK.BaseApp.Router.Types (Application, HasPath (..),
                                                      RouteResult (..),
                                                      RouterError (..))


-- NOTE: most of this was vendored and repurposed from servant

data Router' env a =
    StaticRouter (Map Text (Router' env a)) [env -> a]
  | CaptureRouter (Router' (Text, env) a)
  | Choice (Router' env a) (Router' env a)
  | ChoiceMerge (Router' env a) (Router' env a) (a -> a -> a)


type Router env r req res = Router' env (Application (Sem r) req res)

pathRouter
  :: Text
  -> Router' env a
  -> Router' env a
pathRouter t r = StaticRouter (M.singleton t r) []

leafRouter
  :: (env -> a)
  -> Router' env a
leafRouter l = StaticRouter M.empty [l]

choice
  :: Router' env a
  -> Router' env a
  -> Router' env a
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
  -> Application (Sem r) req res
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
    ChoiceMerge r1 r2 merge ->
      runChoiceMerge [runRouter r1, runRouter r2] merge env req

runChoice
  :: [env -> Application (Sem r) req res]
  -> env
  -> Application (Sem r) req res
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


-- run all routes in a choice, apply a function to merge the results
runChoiceMerge
  :: [env -> Application (Sem r) req res]
  -> (Application (Sem r) req res -> Application (Sem r) req res -> Application (Sem r) req res)
  -> env
  -> Application (Sem r) req res
runChoiceMerge ls merge =
  case ls of
    []       -> \ _ _ -> pure $ Fail PathNotFound
    [r]      -> r
    (r : rs) -> \env -> merge (r env) (runChoiceMerge rs merge env)

-- build a function to merge Router.Application over the same result type, from a merge function on the result type
makeMerge
  :: (res -> res -> res)
  -> Application (Sem r) req res
  -> Application (Sem r) req res
  -> Application (Sem r) req res
makeMerge merge app1 app2 req = do
  response1 <- app1 req
  response2 <- app2 req
  case (response1, response2) of
    (Route a, Route b) -> pure (Route (merge a b))
    (FailFatal _, _)   -> pure response1
    (_, FailFatal _)   -> pure response2
    (Fail _ , _)       -> pure response2
    (_, Fail _)        -> pure response1





