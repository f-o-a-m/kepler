module Tendermint.SDK.Events
  ( Event(..)
  , IsEvent(..)
  , emit

  , EventBuffer
  , newEventBuffer
  , appendEvent
  , flushEventBuffer

  , eval
  ) where

import qualified Control.Concurrent.MVar                as MVar
import           Control.Monad.IO.Class                 
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import qualified Data.List                              as L
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..),
                                                         KVPair (..))
import           Polysemy
import           Polysemy.Output
import           Polysemy.Reader

class IsEvent e where
  makeEventType :: Proxy e -> String
  makeEventData :: e -> [(BS.ByteString, BS.ByteString)]

data EventBuffer = EventBuffer (MVar.MVar [Event])

newEventBuffer :: IO EventBuffer
newEventBuffer = EventBuffer <$> MVar.newMVar []

appendEvent 
  :: Member (Reader EventBuffer) r
  => MonadIO (Sem r)
  => Event
  -> Sem r ()
appendEvent e = do
  EventBuffer b <- ask
  liftIO (MVar.modifyMVar_ b (pure . (e :)))

flushEventBuffer 
  :: Member (Reader EventBuffer) r
  => MonadIO (Sem r)
  => Sem r [Event]
flushEventBuffer = do
  (EventBuffer b) <- ask
  liftIO (L.reverse <$> MVar.swapMVar b [])

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

eval
  :: Member (Embed IO) r
  => Member (Reader EventBuffer) r
  => (forall a. Sem (Output Event ': r) a -> Sem r a)
eval action = interpret (\case
  Output e -> appendEvent e
  ) action
