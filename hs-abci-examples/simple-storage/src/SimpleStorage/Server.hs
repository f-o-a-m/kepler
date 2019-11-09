module SimpleStorage.Server (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.Monoid                                   (Endo (..))
import           Data.Proxy
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (Middleware)
import qualified Network.ABCI.Server.Middleware.RequestLogger  as ReqLogger
import qualified Network.ABCI.Server.Middleware.ResponseLogger as ResLogger
import           SimpleStorage.Application                     (AppConfig,
                                                                AppError,
                                                                Handler,
                                                                runHandler)
import           SimpleStorage.Handlers                        (simpleStorageApp)
import qualified SimpleStorage.Modules.SimpleStorage           as SS
import           Tendermint.SDK.Application                    (MakeApplication (..),
                                                                createApplication)
import           Tendermint.SDK.Router                         (QueryApplication,
                                                                serve)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication cfg = do
  let serveRoutes :: QueryApplication Handler
      serveRoutes = serve (Proxy :: Proxy SS.Api) SS.server
      makeApplication :: MakeApplication Handler AppError
      makeApplication = MakeApplication
        { transformer = runHandler cfg
        , appErrorP = Proxy
        , app = simpleStorageApp serveRoutes
        , initialize = [SS.initialize]
        }
  putStrLn "Starting ABCI application..."
  application <- createApplication makeApplication
  serveApp =<< hookInMiddleware application
  where
    mkMiddleware :: IO (Middleware IO)
    mkMiddleware = do
      reqLogger <- ReqLogger.mkLogStdoutDev
      resLogger <- ResLogger.mkLogStdoutDev
      pure . appEndo . fold $
        [ Endo reqLogger
        , Endo resLogger
        ]
    hookInMiddleware _app = do
      middleware <- mkMiddleware
      pure $ middleware _app
