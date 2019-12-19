module Nameservice.Server (makeAndServeApplication) where

import Data.Monoid (Endo(..))
import Data.Foldable (fold)
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
import           Tendermint.SDK.BaseApp.Metrics.Prometheus     (runMetricsServer)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication AppConfig{..} = do
  putStrLn "Starting ABCI application..."
  runMetricsServer $ contextMetricsConfig baseAppContext
  let nat :: forall a. Sem CoreEffs a -> IO a
      nat = runCoreEffs baseAppContext
      application = makeApp handlersContext
      middleware :: Middleware (Sem CoreEffs)
      middleware = appEndo . fold $
          [ Endo Req.mkRequestLoggerM
          , Endo Res.mkResponseLoggerM
          , Endo $ Met.mkMetricsLoggerM serverMetricsMap
          ]
  serveApp $ createIOApp nat (middleware application)
