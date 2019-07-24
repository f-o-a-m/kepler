module Network.ABCI.Types.App where

import           Control.Natural                      (type (~>))
import           Network.ABCI.Types.Messages.Request  (Request)
import           Network.ABCI.Types.Messages.Response (Response)
import           Network.ABCI.Types.Messages.Types    (MessageType)

newtype App m =
  App (forall (t :: MessageType). Request t -> m (Response t))

transformApp :: (m ~> g) -> App m -> App g
transformApp nat (App f) = App $ nat . f
