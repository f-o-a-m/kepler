module Nameservice.Server (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.Monoid                                   (Endo (..))
import           Nameservice.Application                       (AppConfig (..),
                                                                handlersContext)
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (Middleware)
-- import qualified Network.ABCI.Server.Middleware.MetricsLogger  as MetLogger
import qualified Network.ABCI.Server.Middleware.RequestLogger  as ReqLogger
import qualified Network.ABCI.Server.Middleware.ResponseLogger as ResLogger
import           Polysemy                                      (Sem)
import           Tendermint.SDK.Application                    (createIOApp,
                                                                makeApp)
import           Tendermint.SDK.BaseApp                        (CoreEffs,
                                                                runCoreEffs)
import           Tendermint.SDK.BaseApp.Metrics.Metrics        (initMetricsState)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication cfg = do
  _ <- initMetricsState
  putStrLn "Starting ABCI application..."
  let nat :: forall a. Sem CoreEffs a -> IO a
      nat = runCoreEffs $ baseAppContext cfg
      application = createIOApp nat $ makeApp handlersContext
  serveApp =<< hookInMiddleware application
  where
    mkMiddleware :: IO (Middleware IO)
    mkMiddleware = do
      reqLogger <- ReqLogger.mkLogStdoutDev
      resLogger <- ResLogger.mkLogStdoutDev
      -- metLogger <- MetLogger.mkMetricsLogDatadogLocal 10516 -- local port for log collection (TDP)
      --metLogger <- MetLogger.mkMetricsLogDatadog -- direct datadog
      pure . appEndo . fold $
        [ Endo reqLogger
        , Endo resLogger
        -- , Endo metLogger
        ]
    hookInMiddleware _app = do
      middleware <- mkMiddleware
      pure $ middleware _app
