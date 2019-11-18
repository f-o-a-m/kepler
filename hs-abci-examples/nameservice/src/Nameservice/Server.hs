module Nameservice.Server (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.Monoid                                   (Endo (..))
import           Data.Proxy
import           Nameservice.Application                       (AppConfig (..),
                                                                queryServer,
                                                                runHandler)
import           Nameservice.Handlers                          (nameserviceApp)
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (Middleware)
import qualified Network.ABCI.Server.Middleware.RequestLogger  as ReqLogger
import qualified Network.ABCI.Server.Middleware.ResponseLogger as ResLogger
import           Polysemy                                      (Sem)
import           Tendermint.SDK.Application                    (MakeApplication (..),
                                                                createApplication)
import           Tendermint.SDK.BaseApp                        (BaseApp, eval)
import           Tendermint.SDK.Errors                         (AppError)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication cfg = do
  let makeApplication :: MakeApplication (Sem BaseApp) AppError
      makeApplication = MakeApplication
        { transformer = eval $ baseAppContext cfg
        , appErrorP = Proxy
        , app = nameserviceApp queryServer
        , initialize = []
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
