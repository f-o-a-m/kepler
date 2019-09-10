module Tendermint.SDK.Router where

import           Control.Monad.IO.Class               (MonadIO (..))
import           Data.Proxy

import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response

import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Delayed
import           Tendermint.SDK.Router.Router
import           Tendermint.SDK.Router.Types


serve
  :: HasRouter layout
  => MonadIO m
  => Proxy layout
  -> Proxy m
  -> RouteT layout m
  -> Request.Query
  -> m Response.Query
serve p pm server =
  toApplication (runRouter (route p pm (emptyDelayed (Route server))) ())
  where
    emptyDelayed response =
      let r = pure ()
      in Delayed (const r) $ \_ _ -> response
    toApplication ra query = do
      res <- ra query
      case res of
        Fail e      -> pure $ responseQueryError e
        FailFatal e -> pure $ responseQueryError e
        Route a     -> pure a
