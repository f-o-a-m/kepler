module Tendermint.SDK.BaseApp.Query
  ( serve
  , serveRouter
  , module Tendermint.SDK.BaseApp.Query.Class
  , module Tendermint.SDK.BaseApp.Query.Router
  , module Tendermint.SDK.BaseApp.Query.Types
  , module Tendermint.SDK.BaseApp.Query.Delayed
  , module Tendermint.SDK.BaseApp.Query.Store
  ) where

import           Control.Lens                         ((&), (.~))
import           Data.Default.Class                   (def)
import           Data.Proxy
import           Network.ABCI.Types.Messages.Request  ()
import           Polysemy                             (Sem)
import           Tendermint.SDK.BaseApp.Errors        (makeAppError,
                                                       queryAppError)
import           Tendermint.SDK.BaseApp.Query.Class
import           Tendermint.SDK.BaseApp.Query.Delayed
import           Tendermint.SDK.BaseApp.Query.Router
import           Tendermint.SDK.BaseApp.Query.Store
import           Tendermint.SDK.BaseApp.Query.Types

serveRouter
  :: Router () r
  -> QueryApplication (Sem r)
serveRouter rtr = toApplication $ runRouter rtr ()

serve
  :: HasRouter layout r
  => Proxy layout
  -> Proxy r
  -> RouteT layout r
  -> QueryApplication (Sem r)
serve pl pr server =
  toApplication (runRouter (route pl pr (emptyDelayed (Route server))) ())

toApplication
  :: RoutingApplication r -> QueryApplication (Sem r)
toApplication ra query = do
  res <- ra $ parseQueryRequest query
  case res of
    Fail e      -> pure $ def & queryAppError .~ makeAppError e
    FailFatal e -> pure $ def & queryAppError .~ makeAppError e
    Route a     -> pure a
