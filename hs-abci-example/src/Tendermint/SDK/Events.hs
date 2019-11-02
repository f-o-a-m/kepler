module Tendermint.SDK.Events
  ( Event(..)
  , IsEvent(..)
  , emit

  , EventBuffer
  , newEventBuffer
  , appendEvent
  , flushEventBuffer
  , withEventBuffer

  , evalWithBuffer
  ) where

import qualified Control.Concurrent.MVar                as MVar
import           Control.Monad                          (void)
import           Control.Monad.IO.Class
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import qualified Data.List                              as L
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..),
                                                         KVPair (..))
import           Polysemy                               (Embed, Member, Sem,
                                                         interpret)
import           Polysemy.Output                        (Output (..), output)
import           Polysemy.Resource                      (Resource, onException)

class IsEvent e where
  makeEventType :: Proxy e -> String
  makeEventData :: e -> [(BS.ByteString, BS.ByteString)]

data EventBuffer = EventBuffer (MVar.MVar [Event])

newEventBuffer :: IO EventBuffer
newEventBuffer = EventBuffer <$> MVar.newMVar []

appendEvent
  :: MonadIO (Sem r)
  => Event
  -> EventBuffer
  -> Sem r ()
appendEvent e (EventBuffer b) = do
  liftIO (MVar.modifyMVar_ b (pure . (e :)))

flushEventBuffer
  :: MonadIO (Sem r)
  => EventBuffer
  -> Sem r [Event]
flushEventBuffer (EventBuffer b) = do
  liftIO (L.reverse <$> MVar.swapMVar b [])

withEventBuffer
  :: Member Resource r
  => MonadIO (Sem r)
  => EventBuffer
  -> Sem r ()
  -> Sem r [Event]
withEventBuffer buffer action =
  onException (action *> flushEventBuffer buffer) (void $ flushEventBuffer buffer)

makeEvent
  :: IsEvent e
  => e
  -> Event
makeEvent (e :: e) = Event
  { eventType = cs $ makeEventType (Proxy :: Proxy e)
  , eventAttributes = (\(k, v) -> KVPair (Base64.fromBytes k) (Base64.fromBytes v)) <$> makeEventData e
  }

emit
  :: IsEvent e
  => Member (Output Event) r
  => e
  -> Sem r ()
emit e = output $ makeEvent e

evalWithBuffer
  :: Member (Embed IO) r
  => EventBuffer
  -> (forall a. Sem (Output Event ': r) a -> Sem r a)
evalWithBuffer buffer action = interpret (\case
  Output e -> appendEvent e buffer
  ) action
