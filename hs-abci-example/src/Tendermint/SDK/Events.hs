module Tendermint.SDK.Events
  ( EventKey(..)
  , Event(..)
  , IsEvent(..)
  , emit

  , EventBuffer
  , newEventBuffer
  , appendEvent
  , flushEventBuffer

  , eval
  ) where

import qualified Control.Concurrent.MVar as MVar
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.ByteString         as BS
import           Data.Proxy
import           Data.String.Conversions (cs)
import           GHC.TypeLits            (KnownSymbol, Symbol, symbolVal)
import           Polysemy
import           Polysemy.Output
import           Polysemy.Reader
import           Tendermint.SDK.Codec    (HasCodec (..))

data EventKey = EventKey BS.ByteString deriving (Eq, Show, Ord)

class HasCodec e => IsEvent e where
  type EventName e = (s :: Symbol) | s -> e

data Event = Event
  { eventKey  :: EventKey
  , eventData :: BS.ByteString
  }

data EventBuffer = EventBuffer (MVar.MVar [Event])

newEventBuffer :: IO EventBuffer
newEventBuffer = EventBuffer <$> MVar.newMVar []

appendEvent :: Event -> EventBuffer -> IO ()
appendEvent e (EventBuffer b) = MVar.modifyMVar_ b (pure . (e :))

flushEventBuffer :: EventBuffer -> IO [Event]
flushEventBuffer (EventBuffer b) = MVar.swapMVar b []

makeEvent
  :: IsEvent e
  => KnownSymbol (EventName e)
  => e
  -> Event
makeEvent (e :: e) = Event
  { eventKey = EventKey . cs $ symbolVal (Proxy :: Proxy (EventName e))
  , eventData = encode e
  }

emit
  :: IsEvent e
  => KnownSymbol (EventName e)
  => Member (Output Event) r
  => e
  -> Sem r ()
emit e = output $ makeEvent e

eval
  :: Member (Embed IO) r
  => Member (Reader EventBuffer) r
  => (forall a. Sem (Output Event ': r) a -> Sem r a)
eval action = interpret (\case
  Output e -> do
    eventBuffer <- ask @EventBuffer
    liftIO $ appendEvent e eventBuffer
  ) action
