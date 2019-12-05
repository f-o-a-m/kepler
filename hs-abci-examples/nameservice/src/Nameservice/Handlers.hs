module Nameservice.Handlers where

import           Control.Lens                         (to, (&), (.~), (^.))
import qualified Data.ByteArray.Base64String          as Base64
import           Data.ByteString                      (ByteString)
import           Data.Default.Class                   (def)
import           Nameservice.Application              (router)
import           Network.ABCI.Server.App              (App (..),
                                                       MessageType (..),
                                                       Request (..),
                                                       Response (..))
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Polysemy                             (Sem)
import           Polysemy.Error                       (catch)
import           Tendermint.SDK.Application           (defaultHandler)
import           Tendermint.SDK.BaseApp               (BaseApp, CoreEffs,
                                                       ScopedBaseApp,
                                                       applyScope,
                                                       compileToCoreEff)
import           Tendermint.SDK.Errors                (AppError,
                                                       checkTxAppError,
                                                       deliverTxAppError)
import           Tendermint.SDK.Events                (withEventBuffer)
import           Tendermint.SDK.Query                 (QueryApplication)
import           Tendermint.SDK.Store                 (ConnectionScope (..),
                                                       beginBlock, commitBlock,
                                                       mergeScopes, storeRoot,
                                                       withSandbox,
                                                       withTransaction)
import           Tendermint.SDK.Types.TxResult        (TxResult,
                                                       checkTxTxResult,
                                                       deliverTxTxResult,
                                                       txResultEvents)

echoH
  :: Monad m
  => Request 'MTEcho
  -> m (Response 'MTEcho)
echoH (RequestEcho echo) =
  pure . ResponseEcho $ def & Resp._echoMessage .~ echo ^. Req._echoMessage

flushH
  :: Monad m
  => Request 'MTFlush
  -> m (Response 'MTFlush)
flushH = defaultHandler

infoH
  :: Monad m
  => Request 'MTInfo
  -> m (Response 'MTInfo)
infoH = defaultHandler

setOptionH
  :: Monad m
  => Request 'MTSetOption
  -> m (Response 'MTSetOption)
setOptionH = defaultHandler

-- TODO: this one might be useful for initializing to 0
-- instead of doing that manually in code
initChainH
  :: Monad m
  => Request 'MTInitChain
  -> m (Response 'MTInitChain)
initChainH = defaultHandler

queryH
  :: QueryApplication (Sem BaseApp)
  -> Request 'MTQuery
  -> Sem (ScopedBaseApp 'Query) (Response 'MTQuery)
queryH serveRoutes (RequestQuery query) = applyScope $ do
  queryResp <- serveRoutes query
  pure $ ResponseQuery  queryResp

beginBlockH
  :: Request 'MTBeginBlock
  -> Sem (ScopedBaseApp 'Consensus) (Response 'MTBeginBlock)
beginBlockH _ = def <$ applyScope beginBlock

-- Common function between checkTx and deliverTx
transactionHandler :: ByteString -> Sem BaseApp TxResult
transactionHandler bs = do
  events <- withEventBuffer $ router bs
  pure $ def & txResultEvents .~ events

checkTxH
  :: Request 'MTCheckTx
  -> Sem (ScopedBaseApp 'Mempool) (Response 'MTCheckTx)
checkTxH (RequestCheckTx checkTx) = applyScope $
  let tryToRespond = withSandbox $ do
        txResult <- transactionHandler $ checkTx ^. Req._checkTxTx . to Base64.toBytes
        return $ ResponseCheckTx $ def & checkTxTxResult .~ txResult
  in tryToRespond `catch` \(err :: AppError) ->
       return . ResponseCheckTx $ def & checkTxAppError .~ err


deliverTxH
  :: Request 'MTDeliverTx
  -> Sem (ScopedBaseApp 'Consensus) (Response 'MTDeliverTx) -- Sem BaseApp (Response 'MTDeliverTx)
deliverTxH (RequestDeliverTx deliverTx) = applyScope $
  let tryToRespond = withTransaction $ do
        txResult <- transactionHandler $ deliverTx ^. Req._deliverTxTx . to Base64.toBytes
        return $ ResponseDeliverTx $ def & deliverTxTxResult .~ txResult
  in tryToRespond `catch` \(err :: AppError) ->
       return . ResponseDeliverTx $ def & deliverTxAppError .~ err

endBlockH
  :: Monad m
  => Request 'MTEndBlock
  -> m (Response 'MTEndBlock)
endBlockH = defaultHandler

commitH
  :: Request 'MTCommit
  -> Sem (ScopedBaseApp 'Consensus) (Response 'MTCommit)
commitH _ = applyScope $ do
  commitBlock
  mergeScopes
  rootHash <- storeRoot
  return . ResponseCommit $ def
    & Resp._commitData .~ Base64.fromBytes rootHash


nameserviceApp :: QueryApplication (Sem BaseApp) -> App (Sem CoreEffs)
nameserviceApp serveRoutes = App $ \case
  msg@(RequestEcho _) -> echoH msg
  msg@(RequestFlush _) -> flushH msg
  msg@(RequestInfo _) -> infoH msg
  msg@(RequestSetOption _) -> setOptionH msg
  msg@(RequestInitChain _) -> initChainH msg
  msg@(RequestQuery _) -> compileToCoreEff $ queryH serveRoutes msg
  msg@(RequestBeginBlock _) -> compileToCoreEff $ beginBlockH msg
  msg@(RequestCheckTx _) -> compileToCoreEff $ checkTxH msg
  msg@(RequestDeliverTx _) -> compileToCoreEff $ deliverTxH msg
  msg@(RequestEndBlock _) -> endBlockH msg
  msg@(RequestCommit _) -> compileToCoreEff $ commitH msg
