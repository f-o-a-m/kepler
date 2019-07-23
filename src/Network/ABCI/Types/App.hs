module Network.ABCI.Types.App where

import           Network.ABCI.Types.Messages.Request  (Request)
import           Network.ABCI.Types.Messages.Response (Response)
import           Network.ABCI.Types.Messages.Types    (MessageType)

newtype App m =
  App (forall (t :: MessageType). Request t -> m (Response t))
