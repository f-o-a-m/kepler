module SimpleStorage.App (makeAndServeApplication) where

import           Data.Foldable                                (fold)
import           Data.Monoid                                  (Endo (..))
import           Network.ABCI.Server                          (serveAppWith)
import           Network.ABCI.Server.App                      (App (..),
                                                               Middleware,
                                                               Request (..),
                                                               transformApp)
import           Network.ABCI.Server.Middleware.RequestLogger (mkLogStdoutDev)
import           SimpleStorage.Application                    (Handler,
                                                               makeAppConfig,
                                                               transformHandler)
import           SimpleStorage.Handlers
import           Data.Conduit.Network    (serverSettings)
import Data.String (fromString)

makeAndServeApplication :: IO ()
makeAndServeApplication = do
  cfg <- makeAppConfig
  let ioApp = transformApp (transformHandler cfg) $ app
      s = serverSettings 26658 $ fromString "0.0.0.0"
  putStrLn "Starting ABCI application..."
  serveAppWith s mempty =<< hookInMiddleware ioApp
  where
    mkMiddleware :: IO (Middleware IO)
    mkMiddleware = do
      logger <- mkLogStdoutDev
      pure . appEndo . fold $
        [ Endo logger
        ]
    hookInMiddleware _app = do
      middleware <- mkMiddleware
      pure $ middleware _app

app :: App Handler
app = App $ \case
  msg@(RequestEcho _) -> echoH msg
  msg@(RequestFlush _) -> flushH msg
  msg@(RequestInfo _) -> infoH msg
  msg@(RequestSetOption _) -> setOptionH msg
  msg@(RequestInitChain _) -> initChainH msg
  msg@(RequestQuery _) -> queryH msg
  msg@(RequestBeginBlock _) -> beginBlockH msg
  msg@(RequestCheckTx _) -> checkTxH msg
  msg@(RequestDeliverTx _) -> deliverTxH msg
  msg@(RequestEndBlock _) -> endBlockH msg
  msg@(RequestCommit _) -> commitH msg
