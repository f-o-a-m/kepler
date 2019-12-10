module Tendermint.SDK.BaseApp.Query
  ( serve
  , serveRouter
  , module Tendermint.SDK.BaseApp.Query.Class
  , module Tendermint.SDK.BaseApp.Query.Router
  , module Tendermint.SDK.BaseApp.Query.Types
  , module Tendermint.SDK.BaseApp.Query.Delayed
  , module Tendermint.SDK.BaseApp.Query.Store
  ) where

import           Data.Proxy
import           Tendermint.SDK.BaseApp.Query.Class
import           Tendermint.SDK.BaseApp.Query.Delayed
import           Tendermint.SDK.BaseApp.Query.Router
import           Tendermint.SDK.BaseApp.Query.Store
import           Tendermint.SDK.BaseApp.Query.Types

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
