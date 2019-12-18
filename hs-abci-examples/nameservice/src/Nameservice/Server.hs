module Nameservice.Server (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.Monoid                                   (Endo (..))
import           Nameservice.Application                       (AppConfig (..),
                                                                handlersContext)
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (Middleware)
import qualified Network.ABCI.Server.Middleware.MetricsLogger  as MetLogger
import qualified Network.ABCI.Server.Middleware.RequestLogger  as ReqLogger
import qualified Network.ABCI.Server.Middleware.ResponseLogger as ResLogger
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
      application = createIOApp nat $ makeApp handlersContext
  serveApp =<< hookInMiddleware application
  where
    metCfg = contextMetricsConfig baseAppContext
    apiKey = (fmap MetLogger.ApiKey . metricsAPIKey) =<< metCfg
    mkMiddleware :: IO (Middleware IO)
    mkMiddleware = do
      reqLogger <- ReqLogger.mkLogStdoutDev
      esReqLogger <- ReqLogger.mkLogESDev
      resLogger <- ResLogger.mkLogStdoutDev
      esResLogger <- ResLogger.mkLogESDev
      metLogger <- MetLogger.mkMetricsLogDatadog apiKey
      pure . appEndo . fold $
        [ Endo reqLogger
        , Endo esReqLogger
        , Endo resLogger
        , Endo esResLogger
        , Endo metLogger
        ]
    hookInMiddleware _app = do
      middleware <- mkMiddleware
      pure $ middleware _app
