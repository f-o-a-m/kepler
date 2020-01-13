module Nameservice.Server (makeAndServeApplication) where
import           Control.Lens                              ((^?), _Just)
import           Data.Foldable                             (fold)
import           Data.IORef                                (writeIORef)
import           Data.Monoid                               (Endo (..))
import           Nameservice.Application                   (handlersContext)
import           Nameservice.Config                        (AppConfig (..))
import           Network.ABCI.Server                       (serveApp)
import           Network.ABCI.Server.App                   (Middleware)
import qualified Network.ABCI.Server.Middleware.Logger     as Logger
import qualified Network.ABCI.Server.Middleware.Metrics    as Met
import           Polysemy                                  (Sem)

import           Tendermint.SDK.Application                (createIOApp,
                                                            makeApp)
import           Tendermint.SDK.BaseApp                    (Context (..),
                                                            CoreEffs,
                                                            contextPrometheusEnv,
                                                            runCoreEffs)
import           Tendermint.SDK.BaseApp.Metrics.Prometheus (envMetricsState,
                                                            forkMetricsServer,
                                                            metricsRegistry)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication AppConfig{..} = do
  putStrLn "Starting ABCI application..."
  case _contextPrometheusEnv _baseAppContext of
    Nothing            -> pure ()
    Just prometheusEnv -> do
      prometheusThreadId <- forkMetricsServer prometheusEnv
      writeIORef _prometheusServerThreadId (Just prometheusThreadId)
  metricsMiddleware <-
    case _baseAppContext ^? contextPrometheusEnv . _Just . envMetricsState . metricsRegistry of
      Nothing       -> pure id
      Just registry -> Met.mkMetricsMiddleware Met.defaultBuckets registry

  let nat :: forall a. Sem CoreEffs a -> IO a
      nat = runCoreEffs _baseAppContext
      application = makeApp handlersContext
      middleware :: Middleware (Sem CoreEffs)
      middleware = appEndo . fold $
          [ Endo Logger.mkLoggerM
          , Endo metricsMiddleware
          ]
  serveApp $ createIOApp nat (middleware application)
