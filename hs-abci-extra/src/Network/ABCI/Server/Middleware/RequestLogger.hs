module Network.ABCI.Server.Middleware.RequestLogger
    ( -- * Custom Loggers
      mkRequestLogger
    , mkRequestLoggerM
    ) where

import           Control.Monad.IO.Class  (MonadIO)
import qualified Data.Aeson              as A
import           Katip
import           Network.ABCI.Server.App (App (..), MessageType, Middleware,
                                          Request (..))

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------
-- | Loggable newtype wrapper
newtype Loggable a = Loggable a

instance ToObject (Loggable (Request (t :: MessageType))) where
  toObject (Loggable v) = case A.toJSON v of
      A.Object o -> o
      _          -> error "Contract violation: `toJSON` of any `Request t` must result with json object"

instance LogItem (Loggable (Request (t :: MessageType))) where
  payloadKeys V3 _ = AllKeys
  payloadKeys _ _  = SomeKeys ["type", "event_type"]

---------------------------------------------------------------------------
-- mkRequestLogger
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests with custom 'Katip.LogEnv'
-- and 'Katip.Namespace'. This method makes it easy use various scribes such as
-- <http://hackage.haskell.org/package/katip-elasticsearch-0.5.1.1/docs/Katip-Scribes-ElasticSearch.html elastic-search>.
mkRequestLogger :: (MonadIO m) => LogEnv -> Namespace -> Middleware m
mkRequestLogger le ns (App app) = App $ \ req -> do
  runKatipContextT le () ns $ logRequest req
  app req

---------------------------------------------------------------------------
-- mkRequestLoggerM
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests in app with KatipContext.
-- Great for `App m` with a `KatipContext` instance.
mkRequestLoggerM :: (KatipContext m) => Middleware m
mkRequestLoggerM (App app) = App $ \ req -> logRequest req >> app req

---------------------------------------------------------------------------
-- Common
---------------------------------------------------------------------------
-- | Request logger function.
logRequest :: (KatipContext m) => Request t ->  m ()
logRequest req = localKatipNamespace (<> "server") $
  katipAddContext (Loggable req) $ logFM InfoS "Request Received"
