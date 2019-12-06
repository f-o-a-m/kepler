module Nameservice.Server (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.Monoid                                   (Endo (..))
import           Data.Proxy
import           Nameservice.Application                       (AppConfig (..),
                                                                handlersContext)
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (Middleware)
import qualified Network.ABCI.Server.Middleware.RequestLogger  as ReqLogger
import qualified Network.ABCI.Server.Middleware.ResponseLogger as ResLogger
import           Tendermint.SDK.Application                    (MakeApplication (..),
                                                                createApplication)
import           Tendermint.SDK.BaseApp                        (CoreEffs,
                                                                evalCoreEffs)
import           Tendermint.SDK.Errors                         (AppError)
import           Tendermint.SDK.Handlers                       (makeApp)

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication cfg = do
  let makeApplication :: MakeApplication CoreEffs AppError
      makeApplication = MakeApplication
        { transformer = evalCoreEffs $ baseAppContext cfg
        , appErrorP = Proxy
        , app = makeApp handlersContext
        }
  putStrLn "Starting ABCI application..."
  let application = createApplication makeApplication
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
