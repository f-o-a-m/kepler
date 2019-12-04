module Tendermint.SDK.Query
  ( serve
  , serveRouter
  , QueryApplication
  , module Tendermint.SDK.Query.Class
  , module Tendermint.SDK.Query.Router
  , module Tendermint.SDK.Query.Types
  , module Tendermint.SDK.Query.Delayed
  , module Tendermint.SDK.Query.Store
  ) where

import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Proxy
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Tendermint.SDK.Query.Class
import           Tendermint.SDK.Query.Delayed
import           Tendermint.SDK.Query.Router
import           Tendermint.SDK.Query.Store
import           Tendermint.SDK.Query.Types

type QueryApplication m = Request.Query -> m Response.Query

serveRouter
  :: Monad m
  => Router () m
  -> QueryApplication m
serveRouter r = toApplication $ runRouter r ()

serve
  :: HasRouter layout
  => MonadIO m
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
