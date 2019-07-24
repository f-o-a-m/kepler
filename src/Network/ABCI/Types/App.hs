module Network.ABCI.Types.App
  ( App(..)
  , transformApp
  ) where

import           Network.ABCI.Types.Messages.Request  (Request)
import           Network.ABCI.Types.Messages.Response (Response)
import           Network.ABCI.Types.Messages.Types    (MessageType)

-- | Application type that represents a well typed application, i.e. a
-- function from a typed `Request` to a typed `Response`.
newtype App m = App
  { runApp :: forall (t :: MessageType). Request t -> m (Response t) }

-- | Transform an application from running in a custom monad to running in `IO`.
transformApp
  :: (forall a. m a -> n a)
  -> App m
  -> App n
transformApp nat (App f) = App $ \req ->
  nat $ f req
