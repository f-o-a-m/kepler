module SimpleStorage.Handlers where

import           Control.Lens                         ((&), (.~), (^.))
--import           Control.Monad.IO.Class               (liftIO)
--import           Control.Monad.Reader                 (ask)
--import           Data.Binary                          (encode)
--import           Data.ByteString.Lazy                 (toStrict)
import           Data.Default.Class                   (def)
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Network.ABCI.Types.Messages.Types    (MessageType (..))
import           SimpleStorage.Application            (Handler, defaultHandler)
--import           SimpleStorage.StateMachine           (readCount)


echoH
  :: Req.Request 'MTEcho
  -> Handler (Resp.Response 'MTEcho)
echoH (Req.RequestEcho echo) =
  pure . Resp.ResponseEcho $ def & Resp._echoMessage .~ echo ^. Req._echoMessage


flushH
  :: Req.Request 'MTFlush
  -> Handler (Resp.Response 'MTFlush)
flushH = defaultHandler

infoH
  :: Req.Request 'MTInfo
  -> Handler (Resp.Response 'MTInfo)
infoH = defaultHandler

setOptionH
  :: Req.Request 'MTSetOption
  -> Handler (Resp.Response 'MTSetOption)
setOptionH = defaultHandler

-- TODO: this one might be useful for initializing to 0
-- instead of doing that manually in code
initChainH
  :: Req.Request 'MTInitChain
  -> Handler (Resp.Response 'MTInitChain)
initChainH = defaultHandler

queryH
  :: Req.Request 'MTQuery
  -> Handler (Resp.Response 'MTQuery)
--queryH (Req.RequestQuery Req.Query{queryPath})
--  | queryPath == "count" = handleCountQuery
--  | otherwise = pure . Resp.ResponseQuery $
--      def {Resp.queryCode = fromInteger 1}
--  where
--    handleCountQuery = do
--      AppConfig{countConnection} <- ask
--      count <- liftIO $ readCount countConnection
--      pure . Resp.ResponseQuery $
--        def { Resp.queryCode = fromInteger 0
--            , Resp.queryValue = toStrict . encode $ count
--            }
queryH = defaultHandler
