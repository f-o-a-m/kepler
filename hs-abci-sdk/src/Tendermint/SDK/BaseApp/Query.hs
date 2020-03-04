module Tendermint.SDK.BaseApp.Query
  ( serveQueryApplication
  -- * Re-Exports
  , HasQueryRouter(..)
  , StoreLeaf
  , storeQueryHandler
  , QueryEffs
  , module Tendermint.SDK.BaseApp.Query.Types
  ) where

import           Control.Lens                          ((&), (.~))
import           Data.Default.Class                    (def)
import           Data.Proxy
import qualified Network.ABCI.Types.Messages.Response  as Response
import           Polysemy                              (Sem)
import           Tendermint.SDK.BaseApp.Errors         (makeAppError,
                                                        queryAppError)
import           Tendermint.SDK.BaseApp.Query.Effect   (QueryEffs)
import           Tendermint.SDK.BaseApp.Query.Router   (HasQueryRouter (..))
import           Tendermint.SDK.BaseApp.Query.Store
import           Tendermint.SDK.BaseApp.Query.Types
import           Tendermint.SDK.BaseApp.Router.Delayed (emptyDelayed)
import           Tendermint.SDK.BaseApp.Router.Router  (runRouter)
import           Tendermint.SDK.BaseApp.Router.Types   (Application,
                                                        RouteResult (..))
import           Tendermint.SDK.Types.Effects          ((:&))

serveQueryApplication
  :: HasQueryRouter layout r
  => Proxy layout
  -> Proxy r
  -> RouteQ layout (QueryEffs :& r)
  -> QueryApplication (Sem r)
serveQueryApplication pl pr server =
  toQueryApplication (runRouter (routeQ pl pr (emptyDelayed (Route server))) ())

toQueryApplication
  :: Application (Sem r) QueryRequest Response.Query
  -> QueryApplication (Sem r)
toQueryApplication ra query = do
  res <- ra $ parseQueryRequest query
  case res of
    Fail e      -> pure $ def & queryAppError .~ makeAppError e
    FailFatal e -> pure $ def & queryAppError .~ makeAppError e
    Route a     -> pure a
