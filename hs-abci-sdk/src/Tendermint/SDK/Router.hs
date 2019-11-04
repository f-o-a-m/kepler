module Tendermint.SDK.Router
  ( serve
  , serveRouter
  , QueryApplication
  , module Tendermint.SDK.Router.Class
  , module Tendermint.SDK.Router.Router
  , module Tendermint.SDK.Router.Types
  , module Tendermint.SDK.Router.Delayed
  ) where

import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Proxy

import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response

import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Delayed
import           Tendermint.SDK.Router.Router
import           Tendermint.SDK.Router.Types

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
    Fail e      -> pure $ responseQueryError e
    FailFatal e -> pure $ responseQueryError e
    Route a     -> pure a
