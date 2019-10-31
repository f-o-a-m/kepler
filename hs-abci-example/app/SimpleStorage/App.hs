module SimpleStorage.App (makeAndServeApplication) where

import           Data.Foldable                                 (fold)
import           Data.Monoid                                   (Endo (..))
import           Network.ABCI.Server                           (serveApp)
import           Network.ABCI.Server.App                       (App (..),
                                                                Middleware,
                                                                Request (..),
                                                                transformApp)
import qualified Network.ABCI.Server.Middleware.RequestLogger  as ReqLogger
import qualified Network.ABCI.Server.Middleware.ResponseLogger as ResLogger
import           SimpleStorage.Application                     (AppConfig,
                                                                Handler,
                                                                transformHandler)
import           SimpleStorage.Handlers
import  qualified SimpleStorage.Modules.SimpleStorage as SS
import Tendermint.SDK.Router
import Data.Proxy

makeAndServeApplication :: AppConfig -> IO ()
makeAndServeApplication cfg = do
  let serveRoutes = serve (Proxy :: Proxy SS.Api) SS.server
      application = transformApp (transformHandler cfg) $ app serveRoutes
  putStrLn "Starting ABCI application..."
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
