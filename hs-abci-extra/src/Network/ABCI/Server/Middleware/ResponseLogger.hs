module Network.ABCI.Server.Middleware.ResponseLogger
    ( -- * Custom Loggers
      mkResponseLogger
    , mkResponseLoggerM
    ) where
import           Control.Monad.IO.Class  (MonadIO)
import qualified Data.Aeson              as A
import           Katip
import           Network.ABCI.Server.App (App (..), MessageType, Middleware,
                                          Response (..))

---------------------------------------------------------------------------
-- Types
---------------------------------------------------------------------------
-- | Loggable newtype wrapper
newtype Loggable a = Loggable a

instance ToObject (Loggable (Response (t :: MessageType))) where
  toObject (Loggable v) = case A.toJSON v of
      A.Object o -> o
      _          -> error "Contract violation: `toJSON` of any `Response t` must result with json object"

instance LogItem (Loggable (Response (t :: MessageType))) where
  payloadKeys V3 _ = AllKeys
  payloadKeys _ _  = SomeKeys ["type"]

---------------------------------------------------------------------------
-- mkResponseLogger
---------------------------------------------------------------------------
-- | Response logger middleware for ABCI requests with custom 'Katip.LogEnv'
-- and 'Katip.Namespace'. This method makes it easy use various scribes such as
-- <http://hackage.haskell.org/package/katip-elasticsearch-0.5.1.1/docs/Katip-Scribes-ElasticSearch.html elastic-search>.
mkResponseLogger :: (MonadIO m) => LogEnv -> Namespace -> Middleware m
mkResponseLogger le ns (App app) = App $ \ req -> do
  res <- app req
  runKatipContextT le () ns $ logResponse res
  pure res

---------------------------------------------------------------------------
-- mkResponseLoggerM
---------------------------------------------------------------------------
-- | Response logger middleware for ABCI requests in app with KatipContext.
-- Great for `App m` with a `KatipContext` instance.
mkResponseLoggerM :: (KatipContext m) => Middleware m
mkResponseLoggerM (App app) = App $ \ req -> do
  res <- app req
  logResponse res
  pure res

---------------------------------------------------------------------------
-- Common
---------------------------------------------------------------------------
-- | Response logger function.
logResponse :: (KatipContext m) => Response t ->  m ()
logResponse resp =
  let logLevel = case resp of
        ResponseFlush _ -> DebugS
        ResponseEcho _ -> DebugS
        _ -> InfoS
  in localKatipNamespace (<> "server") $
       katipAddContext (Loggable resp) $ logFM logLevel "Response Sent"