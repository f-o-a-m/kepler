module SimpleStorage.Handlers where

import           Control.Concurrent.STM               (atomically)
import           Control.Concurrent.STM.TVar          (readTVar)
import           Control.Lens                         (to, (&), (.~), (^.))
import           Control.Monad.Except                 (throwError)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Reader                 (ask)
import           Data.Binary                          (encode)
import           Data.ByteArray                       (convert)
import           Data.ByteString.Lazy                 (toStrict)
import           Data.Default.Class                   (def)
import           Network.ABCI.Types.App (Request(..),Response(..), MessageType (..))
import qualified Network.ABCI.Types.Messages.Request as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import           SimpleStorage.Application            (AppConfig (..),
                                                       AppError (..), Handler,
                                                       defaultHandler)
import           SimpleStorage.StateMachine           (readCount, updateCount)
import           SimpleStorage.Types                  (AppTxMessage (..),
                                                       decodeAppTxMessage)
import           Tendermint.SDK.DB                    (Connection (..))
import           Tendermint.SDK.Transaction           (commitTransaction,
                                                       stageTransaction)

echoH
  :: Request 'MTEcho
  -> Handler (Response 'MTEcho)
echoH (RequestEcho echo) =
  pure . ResponseEcho $ def & Resp._echoMessage .~ echo ^. Req._echoMessage

flushH
  :: Request 'MTFlush
  -> Handler (Response 'MTFlush)
flushH = defaultHandler

infoH
  :: Request 'MTInfo
  -> Handler (Response 'MTInfo)
infoH = defaultHandler

setOptionH
  :: Request 'MTSetOption
  -> Handler (Response 'MTSetOption)
setOptionH = defaultHandler

-- TODO: this one might be useful for initializing to 0
-- instead of doing that manually in code
initChainH
  :: Request 'MTInitChain
  -> Handler (Response 'MTInitChain)
initChainH = defaultHandler

queryH
  :: Request 'MTQuery
  -> Handler (Response 'MTQuery)
queryH (RequestQuery query)
  | query ^. Req._queryPath == "count" = handleCountQuery
  | otherwise = pure . ResponseQuery $
      def & Resp._queryCode .~ 1
  where
    handleCountQuery = do
      AppConfig{countConnection} <- ask
      count <- liftIO $ readCount countConnection
      let countBS = toStrict . encode $ count
      pure . ResponseQuery $
        def & Resp._queryCode .~ 0
            & Resp._queryValue .~ convert countBS

beginBlockH
  :: Request 'MTBeginBlock
  -> Handler (Response 'MTBeginBlock)
beginBlockH = defaultHandler

checkTxH
  :: Request 'MTCheckTx
  -> Handler (Response 'MTCheckTx)
checkTxH (RequestCheckTx checkTx) = do
  case decodeAppTxMessage $ checkTx ^. Req._checkTxTx . to convert of
    Left e -> throwError $ DecodeTxError e
    Right (ATMUpdateCount updateCountTx) -> do
      AppConfig{countConnection} <- ask
      eRes <- liftIO $ atomically $ do
        let Connection c = countConnection
        db <- readTVar c
        pure $ stageTransaction db $
          updateCount updateCountTx
      return . ResponseCheckTx $ case eRes of
        Left _  -> def & Resp._checkTxCode .~ 1
        Right _ -> def & Resp._checkTxCode .~ 0

deliverTxH
  :: Request 'MTDeliverTx
  -> Handler (Response 'MTDeliverTx)
deliverTxH (RequestDeliverTx deliverTx) = do
  case decodeAppTxMessage $ deliverTx ^. Req._deliverTxTx . to convert of
    Left e -> throwError $ DecodeTxError e
    Right (ATMUpdateCount updateCountTx) -> do
      AppConfig{countConnection} <- ask
      eRes <- liftIO $ commitTransaction countConnection $
        updateCount updateCountTx
      return . ResponseDeliverTx $ case eRes of
        Left _  -> def & Resp._deliverTxCode .~ 1
        Right _ -> def & Resp._deliverTxCode .~ 0

endBlockH
  :: Request 'MTEndBlock
  -> Handler (Response 'MTEndBlock)
endBlockH = defaultHandler

commitH
  :: Request 'MTCommit
  -> Handler (Response 'MTCommit)
commitH = defaultHandler
