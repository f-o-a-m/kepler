module SimpleStorage.App (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.Monoid                                   (Endo (..))
import           Data.Proxy
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (App (..),
                                                                Middleware,
                                                                Request (..))
import qualified Network.ABCI.Server.Middleware.RequestLogger  as ReqLogger
import qualified Network.ABCI.Server.Middleware.ResponseLogger as ResLogger
import           SimpleStorage.Application                     (AppConfig,
                                                                Handler, AppError,
                                                                runHandler)
import           SimpleStorage.Handlers
import qualified SimpleStorage.Modules.SimpleStorage           as SS
import           Tendermint.SDK.Router
import           Tendermint.SDK.Application

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication cfg = do
  let serveRoutes :: QueryApplication Handler
      serveRoutes = serve (Proxy :: Proxy SS.Api) SS.server
      makeApplication :: MakeApplication Handler AppError
      makeApplication = MakeApplication
        { transformer = runHandler cfg
        , appErrorP = Proxy
        , app = app serveRoutes
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

    app :: QueryApplication Handler -> App Handler
    app serveRoutes = App $ \case
      msg@(RequestEcho _) -> echoH msg
      msg@(RequestFlush _) -> flushH msg
      msg@(RequestInfo _) -> infoH msg
      msg@(RequestSetOption _) -> setOptionH msg
      msg@(RequestInitChain _) -> initChainH msg
      msg@(RequestQuery _) -> queryH serveRoutes msg
      msg@(RequestBeginBlock _) -> beginBlockH msg
      msg@(RequestCheckTx _) -> checkTxH msg
      msg@(RequestDeliverTx _) -> deliverTxH msg
      msg@(RequestEndBlock _) -> endBlockH msg
      msg@(RequestCommit _) -> commitH msg
