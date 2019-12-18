module Network.ABCI.Server.Middleware.ResponseLogger
    ( -- * Basic stdout logging
      mkLogStdout
    , mkLogStdoutDev
    , mkLogESDev
      -- * Custom Loggers
    , mkResponseLogger
    , mkResponseLoggerM
    ) where
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Aeson              as A
import           Katip
import           Network.ABCI.Server.App (App (..), MessageType, Middleware,
                                          Response (..))
import           System.IO               (stdout)
import Katip.Scribes.ElasticSearch
import Database.V5.Bloodhound
import qualified Network.HTTP.Client as Client

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
  payloadKeys V0 _ = SomeKeys ["type"]
  payloadKeys _ _  = AllKeys

---------------------------------------------------------------------------
-- mkLogStdout
--------------------------------------------------------------------------
-- | Creates a production request logger as middleware for ABCI requests.
-- Uses Lowest possible verbosity.
mkLogStdout :: (MonadIO m) => m (Middleware m)
mkLogStdout = do
  handleScribe <- liftIO $ mkHandleScribe ColorIfTerminal stdout (permitItem InfoS) V0
  le <- liftIO (registerScribe "stdout" handleScribe defaultScribeSettings
        =<< initLogEnv "ABCI" "production")
  let ns = "Server"
  pure $ mkResponseLogger le ns

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
  pure $ mkResponseLogger le ns

---------------------------------------------------------------------------
-- mkLogESDev
--------------------------------------------------------------------------
mkLogESDev :: (MonadIO m) => m (Middleware m)
mkLogESDev = do
  mgr <- liftIO $ Client.newManager Client.defaultManagerSettings
  let bhe = mkBHEnv (Server "http://localhost:9201") mgr
  esScribe <- liftIO $ mkEsScribe
    -- Reasonable for production
    defaultEsScribeCfgV5
    -- Reasonable for single-node in development
    bhe
    (IndexName "nameservice")
    (MappingName "application-logs")
    (permitItem DebugS)
    V3
  le <- liftIO $ registerScribe "es" esScribe defaultScribeSettings
        =<< initLogEnv "ABCI" "production"
  let ns = "Server"
  pure $ mkResponseLogger le ns


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
logResponse req = katipAddContext (Loggable req) $ logFM InfoS "Response Received"
