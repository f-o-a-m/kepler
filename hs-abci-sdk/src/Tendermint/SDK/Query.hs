module Tendermint.SDK.Query
  ( serve
  , serveRouter
  , module Tendermint.SDK.Query.Class
  , module Tendermint.SDK.Query.Router
  , module Tendermint.SDK.Query.Types
  , module Tendermint.SDK.Query.Delayed
  , module Tendermint.SDK.Query.Store
  ) where

import           Data.Proxy
import           Tendermint.SDK.Query.Class
import           Tendermint.SDK.Query.Delayed
import           Tendermint.SDK.Query.Router
import           Tendermint.SDK.Query.Store
import           Tendermint.SDK.Query.Types

serveRouter
  :: Monad m
  => Router () m
  -> QueryApplication m
serveRouter r = toApplication $ runRouter r ()

serve
  :: HasRouter layout
  => Monad m
  => Proxy layout
  -> RouteT layout m
  -> QueryApplication m
serve p server =
  toApplication (runRouter (route p (emptyDelayed (Route server))) ())

toApplication
  :: Monad m
  => RoutingApplication m -> QueryApplication m
toApplication ra query = do
  res <- ra query
  case res of
    Fail e      -> pure $ responseQueryError query e
    FailFatal e -> pure $ responseQueryError query e
    Route a     -> pure a
