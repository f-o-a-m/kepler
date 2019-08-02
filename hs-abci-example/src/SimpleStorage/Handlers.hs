module SimpleStorage.Handlers where

import           SimpleStorage.Application            (Handler, defaultHandler)
--import SimpleStorage.StateMachine (updateCount, readCount)
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import           Network.ABCI.Types.Messages.Types    (MessageType (..))

infoH :: Req.Request 'MTInfo -> Handler (Resp.Response 'MTInfo)
infoH = defaultHandler
