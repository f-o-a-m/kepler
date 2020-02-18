module Tendermint.SDK.BaseApp.Events
  ( module Tendermint.SDK.BaseApp.Events
  -- Re-Exports
  , Event(..)
  ) where

import qualified Data.Aeson                             as A
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import           Data.Int                               (Int64)
import           Data.String.Conversions                (ConvertibleStrings (..),
                                                         cs)
import           Data.Text                              (Text)
import           GHC.Generics
import           Network.ABCI.Types.Messages.FieldTypes (Event (..),
                                                         KVPair (..))
import           Polysemy                               (Member, Sem)
import           Polysemy.Output                        (Output, output)
import qualified Tendermint.SDK.BaseApp.Logger          as Log
import           Tendermint.SDK.Codec                   (HasCodec (..))

{-
TODO : These JSON instances are fragile but convenient. We
should come up with a custom solution.
-}

class GToNamedEventPrimatives f where
  gtoNamedEventPrimatives :: f a -> [(BS.ByteString, BS.ByteString)]

instance (GToNamedEventPrimatives f) => GToNamedEventPrimatives (C1 c f) where
  gtoNamedEventPrimatives = gtoNamedEventPrimatives . unM1

instance (Selector s, HasCodec a) => GToNamedEventPrimatives (S1 s (K1 i a)) where
  gtoNamedEventPrimatives m1@(M1 x) =
    let name = cs $ selName m1
        val = encode $ unK1 x
    in [(name, val)]

instance (GToNamedEventPrimatives a, GToNamedEventPrimatives b) => GToNamedEventPrimatives (a :*: b) where
  gtoNamedEventPrimatives (a :*: b) = gtoNamedEventPrimatives a <> gtoNamedEventPrimatives b

class GToEvent1 f where
  gmakeEvent1 :: f p -> Event

instance (GToNamedEventPrimatives f, Datatype d) => GToEvent1 (D1 d f) where
  gmakeEvent1 m1@(M1 x) = Event
    { eventType = cs $ datatypeName m1
    , eventAttributes = (\(k, v) -> KVPair (Base64.fromBytes k) (Base64.fromBytes v)) <$> gtoNamedEventPrimatives x
    }

-- | A class representing a type that can be emitted as an event in the
-- | event logs for the deliverTx response.
class ToEvent e where
  toEvent :: e -> Event

  default toEvent :: (Generic e, GToEvent1 (Rep e)) => e -> Event
  toEvent = gmakeEvent1 . from

emit
  :: ToEvent e
  => Member (Output Event) r
  => e
  -> Sem r ()
emit e = output $ toEvent e



-- | Special event wrapper to add contextual event_type info
newtype ContextEvent t = ContextEvent t
instance (A.ToJSON a, ToEvent a) => A.ToJSON (ContextEvent a) where
  toJSON (ContextEvent a) =
    let Event{eventType} = toEvent a
    in  A.object [ "event_type" A..= eventType
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
  let Event{eventType} = toEvent event
  in Log.log Log.Info eventType
