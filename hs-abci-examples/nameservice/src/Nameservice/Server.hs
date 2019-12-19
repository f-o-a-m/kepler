module Nameservice.Server (makeAndServeApplication) where

import           Nameservice.Application                       (AppConfig (..),
                                                                handlersContext)
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (Middleware)
import qualified Network.ABCI.Server.Middleware.MetricsLogger  as Met
import qualified Network.ABCI.Server.Middleware.RequestLogger  as Req
import qualified Network.ABCI.Server.Middleware.ResponseLogger as Res
import           Polysemy                                      (Sem)
import           Tendermint.SDK.Application                    (createIOApp,
                                                                makeApp)
import           Tendermint.SDK.BaseApp                        (Context (..),
                                                                CoreEffs,
                                                                runCoreEffs)
import           Tendermint.SDK.BaseApp.Metrics.Prometheus     (MetricsConfig (..),
                                                                runMetricsServer)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication AppConfig{..} = do
  putStrLn "Starting ABCI application..."
  runMetricsServer metCfg
  let nat :: forall a. Sem CoreEffs a -> IO a
      nat = runCoreEffs baseAppContext
      application = createIOApp nat . addContextAppLoggers $
        makeApp handlersContext
  serveApp =<< hookInMiddleware application
  where
    metCfg = contextMetricsConfig baseAppContext
    apiKey = (fmap Met.ApiKey . metricsAPIKey) =<< metCfg
    -- response/request use the provided log environment from CoreEffs
    addContextAppLoggers = Res.mkResponseLoggerM . Req.mkRequestLoggerM
    -- direct to datadog logger requires a different log environment
    mkMetricMiddleware :: IO (Middleware IO)
    mkMetricMiddleware = pure =<< Met.mkMetricsLogDatadog apiKey
    hookInMiddleware _app = do
      middleware <- mkMetricMiddleware
      pure $ middleware _app
