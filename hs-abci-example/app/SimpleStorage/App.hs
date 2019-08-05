module SimpleStorage.App (makeAndServeApplication) where

import           Network.ABCI                        (serveApp)
import           Network.ABCI.Types.App              (App (..), transformApp)
import qualified Network.ABCI.Types.Messages.Request as Req
import           SimpleStorage.Application           (Handler, makeAppConfig,
                                                      transformHandler)
import           SimpleStorage.Handlers

makeAndServeApplication :: IO ()
makeAndServeApplication = do
  cfg <- makeAppConfig
  let ioApp = transformApp (transformHandler cfg) $ app
  serveApp ioApp

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
