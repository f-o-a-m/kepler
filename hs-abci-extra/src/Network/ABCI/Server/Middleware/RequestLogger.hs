module Network.ABCI.Server.Middleware.RequestLogger
    ( -- * Basic stdout logging
      mkLogStdout
    , mkLogStdoutDev
      -- * Custom Loggers
    , mkRequestLogger
    , mkRequestLoggerM
    ) where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Aeson              as A
import           Katip
import           Network.ABCI.Server.App (App (..), MessageType, Middleware,
                                          Request (..))
import           System.IO               (stdout)

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
-- mkLogStdout
--------------------------------------------------------------------------
-- | Creates a production request logger as middleware for ABCI requests.
-- Uses lowest possible verbosity.
mkLogStdout :: (MonadIO m) => m (Middleware m)
mkLogStdout = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V0
  le <- liftIO (registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "ABCI" "production")
  let ns = "Server"
  pure $ mkRequestLogger le ns

---------------------------------------------------------------------------
-- mkLogStdoutDev
--------------------------------------------------------------------------
-- | Creates a request logger as middleware for ABCI requests.
-- Uses highest possible verbosity.
mkLogStdoutDev :: (MonadIO m) => m (Middleware m)
mkLogStdoutDev = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem DebugS) V3
  le <- liftIO (registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "ABCI" "development")
  let ns = "Server"
  pure $ mkRequestLogger le ns

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
