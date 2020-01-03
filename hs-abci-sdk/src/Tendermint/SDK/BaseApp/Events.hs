module Tendermint.SDK.BaseApp.Events
  ( Event(..)
  , ToEvent(..)
  , ContextEvent(..)
  , emit
  , logEvent
  , makeEvent
  , EventBuffer
  , newEventBuffer
  , withEventBuffer
  , evalWithBuffer
  ) where

import qualified Control.Concurrent.MVar                as MVar
import           Control.Monad                          (void)
import           Control.Monad.IO.Class
import qualified Data.Aeson                             as A
import           Data.Bifunctor                         (bimap)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import qualified Data.List                              as L
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           GHC.Exts                               (toList)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..),
                                                         KVPair (..))
import           Polysemy                               (Embed, Member, Sem,
                                                         interpret)
import           Polysemy.Output                        (Output (..), output)
import           Polysemy.Reader                        (Reader (..), ask)
import           Polysemy.Resource                      (Resource, onException)
import qualified Tendermint.SDK.BaseApp.Logger          as Log

{-
TODO : These JSON instances are fragile but convenient. We
should come up with a custom solution.
-}

-- | A class representing a type that can be emitted as an event in the
-- | event logs for the deliverTx response.
class ToEvent e where
  makeEventType :: Proxy e -> String
  makeEventData :: e -> [(BS.ByteString, BS.ByteString)]

  default makeEventData :: A.ToJSON e => e -> [(BS.ByteString, BS.ByteString)]
  makeEventData e = case A.toJSON e of
    A.Object obj -> bimap cs (cs . A.encode) <$> toList obj
    _            -> mempty

-- This is the internal implementation of the interpreter for event
-- logging. We allocate a buffer that can queue events as they are thrown,
-- then flush the buffer at the end of transaction execution. It will
-- also flush in the event that exceptions are thrown.

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
  => Member (Reader EventBuffer) r
  => MonadIO (Sem r)
  => Sem r ()
  -> Sem r [Event]
withEventBuffer action = do
  buffer <- ask
  onException (action *> flushEventBuffer buffer) (void $ flushEventBuffer buffer)

makeEvent
  :: ToEvent e
  => e
  -> Event
makeEvent (e :: e) = Event
  { eventType = cs $ makeEventType (Proxy :: Proxy e)
  , eventAttributes = (\(k, v) -> KVPair (Base64.fromBytes k) (Base64.fromBytes v)) <$> makeEventData e
  }

emit
  :: ToEvent e
  => Member (Output Event) r
  => e
  -> Sem r ()
emit e = output $ makeEvent e

-- | Special event wrapper to add contextual event_type info
newtype ContextEvent t = ContextEvent t
instance (A.ToJSON a, ToEvent a) => A.ToJSON (ContextEvent a) where
  toJSON (ContextEvent a) =
    A.object [ "event_type" A..= makeEventType (Proxy :: Proxy a)
             , "event" A..= A.toJSON a
             ]
instance Log.Select a => Log.Select (ContextEvent a) where
  select v (ContextEvent a) = Log.select v a

logEvent
  :: forall e r.
     (A.ToJSON e, ToEvent e, Log.Select e)
  => Member Log.Logger r
  => e
  -> Sem r ()
logEvent event = Log.addContext (ContextEvent event) $
  Log.log Log.Info (cs $ makeEventType (Proxy :: Proxy e))

evalWithBuffer
  :: Member (Embed IO) r
  => Member (Reader EventBuffer) r
  => (forall a. Sem (Output Event ': r) a -> Sem r a)
evalWithBuffer action = interpret (\case
  Output e -> ask >>= appendEvent e
  ) action
