module SimpleStorage.Server (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.IORef                                    (writeIORef)
import           Data.Monoid                                   (Endo (..))
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (Middleware)
import qualified Network.ABCI.Server.Middleware.MetricsLogger  as Met
import qualified Network.ABCI.Server.Middleware.RequestLogger  as Req
import qualified Network.ABCI.Server.Middleware.ResponseLogger as Res
import           Polysemy                                      (Sem)
import           SimpleStorage.Application                     (handlersContext)
import           SimpleStorage.Config                          (AppConfig (..))
import           Tendermint.SDK.Application                    (createIOApp,
                                                                makeApp)
import           Tendermint.SDK.BaseApp                        (Context (..),
                                                                CoreEffs,
                                                                runCoreEffs)
import           Tendermint.SDK.BaseApp.Metrics.Prometheus     (forkMetricsServer)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication AppConfig{..} = do
  putStrLn "Starting ABCI application..."
  case _contextPrometheusEnv _baseAppContext of
    Nothing            -> pure ()
    Just prometheusEnv -> do
      prometheusThreadId <- forkMetricsServer prometheusEnv
      writeIORef _prometheusServerThreadId (Just prometheusThreadId)
  let nat :: forall a. Sem CoreEffs a -> IO a
      nat = runCoreEffs _baseAppContext
      application = makeApp handlersContext
      middleware :: Middleware (Sem CoreEffs)
      middleware = appEndo . fold $
          [ Endo Req.mkRequestLoggerM
          , Endo Res.mkResponseLoggerM
          , Endo $ Met.mkMetricsLoggerM _serverMetricsMap
          ]
  serveApp $ createIOApp nat (middleware application)
