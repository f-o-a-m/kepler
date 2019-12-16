module Nameservice.Server (makeAndServeApplication) where

import           Control.Monad                                 (unless)
import           Data.Foldable                                 (fold)
import           Data.Maybe                                    (fromJust,
                                                                isNothing)
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
  unless (isNothing metCfg) (do
    runMetricsServer . fromJust $ metCfg
    )
  putStrLn "Starting ABCI application..."
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
      resLogger <- ResLogger.mkLogStdoutDev
      metLogger <- MetLogger.mkMetricsLogDatadog apiKey
      pure . appEndo . fold $
        [ Endo reqLogger
        , Endo resLogger
        , Endo metLogger
        ]
    hookInMiddleware _app = do
      middleware <- mkMiddleware
      pure $ middleware _app
