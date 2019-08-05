module SimpleStorage.App (makeAndServeApplication) where

import           Network.ABCI                        (serveApp)
import           Network.ABCI.Types.App              (App (..), transformApp, Middleware)
import qualified Network.ABCI.Types.Messages.Request as Req
import           SimpleStorage.Application           (Handler, makeAppConfig,
                                                      transformHandler)
import Data.Foldable (fold)
import           SimpleStorage.Handlers
import           Data.Monoid (Endo(..))
import            Network.ABCI.Middleware.RequestLogger (mkLogStdout)

makeAndServeApplication :: IO ()
makeAndServeApplication = do
  cfg <- makeAppConfig
  let ioApp = transformApp (transformHandler cfg) $ app
  serveApp =<< hookInMiddleware ioApp
  where
    mkMiddleware :: IO (Middleware IO)
    mkMiddleware = do
      logger <- mkLogStdout
      pure . appEndo . fold $
        [ Endo logger
        ]
    hookInMiddleware _app = do
      middleware <- mkMiddleware
      pure $ middleware _app

app :: App Handler
app = App $ \case
  msg@(Req.RequestEcho _) -> echoH msg
  msg@(Req.RequestFlush _) -> flushH msg
  msg@(Req.RequestInfo _) -> infoH msg
  msg@(Req.RequestSetOption _) -> setOptionH msg
  msg@(Req.RequestInitChain _) -> initChainH msg
  msg@(Req.RequestQuery _) -> queryH msg
  msg@(Req.RequestBeginBlock _) -> beginBlockH msg
  msg@(Req.RequestCheckTx _) -> checkTxH msg
  msg@(Req.RequestDeliverTx _) -> deliverTxH msg
  msg@(Req.RequestEndBlock _) -> endBlockH msg
  msg@(Req.RequestCommit _) -> commitH msg
