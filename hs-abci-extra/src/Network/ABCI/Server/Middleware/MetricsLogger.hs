module Network.ABCI.Server.Middleware.MetricsLogger
    ( -- * Basic stdout logging
      mkLogStdout
    , mkLogStdoutDev
      -- * Custom Loggers
    , mkMetricsLogger
    , mkMetricsLoggerM
    ) where
import           Control.Lens            (at, (?~))
import           Control.Monad.IO.Class  (MonadIO, liftIO)
import qualified Data.Aeson              as A
import qualified Data.HashMap.Strict     as H
import           Data.Text               (Text)
import           Katip
import           Network.ABCI.Server.App (App (..), MessageType, Middleware,
                                          Request (..))
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           System.IO               (stdout)

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
  mvarReqC <- liftIO $ newMVar initRequestCounter
  pure $ mkMetricsLogger mvarReqC le ns

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
  mvarReqC <- liftIO $ newMVar initRequestCounter
  pure $ mkMetricsLogger mvarReqC le ns

---------------------------------------------------------------------------
-- mkRequestLogger
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests with custom 'Katip.LogEnv'
-- and 'Katip.Namespace'. This method makes it easy use various scribes such as
-- <http://hackage.haskell.org/package/katip-elasticsearch-0.5.1.1/docs/Katip-Scribes-ElasticSearch.html elastic-search>.
mkMetricsLogger :: (MonadIO m) => MVar (Map MessageType Integer) -> LogEnv -> Namespace -> Middleware m
mkMetricsLogger mvarMap le ns (App app) = App $ \ req -> do
  runKatipContextT le () ns $ logMetrics req
  app req

---------------------------------------------------------------------------
-- mkRequestLoggerM
---------------------------------------------------------------------------
-- | Request logger middleware for ABCI requests in app with KatipContext.
-- Great for `App m` with a `KatipContext` instance.
mkMetricsLoggerM :: (KatipContext m) => Middleware m
mkMetricsLoggerM (App app) = App $ \ req -> logMetrics req >> app req

---------------------------------------------------------------------------
-- Common
---------------------------------------------------------------------------
-- | Request logger function.
logMetrics :: (KatipContext m) => Request t -> m ()
logMetrics req =  logFM InfoS "Request Received"

---------------------------------------------------------------------------
-- initRequestCounter
---------------------------------------------------------------------------
initRequestCounter :: Map MessageType Integer
initRequestCounter = undefined

---------------------------------------------------------------------------
-- initRequestCounter
---------------------------------------------------------------------------
