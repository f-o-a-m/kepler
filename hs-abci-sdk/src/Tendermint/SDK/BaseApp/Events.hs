module Tendermint.SDK.BaseApp.Events
  (
  -- * Class
    ToEvent(..)
  , ContextEvent(..)
  , makeEvent

  -- * Effect
  , emit
  , logEvent

  -- * Re-Exports
  , Event(..)
  ) where

import qualified Data.Aeson                             as A
import           Data.Bifunctor                         (bimap)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import           Data.Proxy
import           Data.String.Conversions                (cs, ConvertibleStrings(..))
import           Data.Int                                    (Int64)
import Data.Text (Text)
import           GHC.Exts                               (toList)
import GHC.Generics
import qualified Data.Map.Strict as Map
import           Network.ABCI.Types.Messages.FieldTypes (Event (..),
                                                         KVPair (..))
import           Polysemy                               (Member, Sem)
import           Polysemy.Output                        (Output, output)
import qualified Tendermint.SDK.BaseApp.Logger          as Log

data EventValue = EVBytes BS.ByteString
                | EVInt Int64
                | EVString Text
  deriving (Eq, Show)

instance ConvertibleStrings EventValue BS.ByteString where
  convertString (EVBytes bs) = cs bs
  convertString (EVInt int) = cs $ show int
  convertString (EVString t) = cs t


{-
TODO : These JSON instances are fragile but convenient. We
should come up with a custom solution.
-}

data EventPrimative = 
  EventInt Integer | EventBytes BS.ByteString | EventString Text

encodeEventPrimative :: EventPrimative -> Base64.Base64String
encodeEventPrimative = \case
  EventInt int -> Base64.fromBytes . cs @_ @BS.ByteString . show $ int
  EventBytes bytes-> Base64.fromBytes bytes
  EventString str -> Base64.fromBytes . cs @_ @BS.ByteString $ str

class ToEventPrimative a  where
  toEventPrimative :: a -> EventPrimative

class GToNamedEventPrimatives f where
  gtoNamedEventPrimatives :: f a -> [(BS.ByteString, EventPrimative)]

instance (GToNamedEventPrimatives f) => GToNamedEventPrimatives (C1 c f) where
  gtoNamedEventPrimatives = gtoNamedEventPrimatives . unM1

instance (Selector s, ToEventPrimative a) => GToNamedEventPrimatives (S1 s (K1 i a)) where
  gtoNamedEventPrimatives m1@(M1 x) = 
    let name = cs $ selName m1
        val = toEventPrimative $ unK1 x
    in [(name, val)]

instance (GToNamedEventPrimatives a, GToNamedEventPrimatives b) => GToNamedEventPrimatives (a :*: b) where
  gtoNamedEventPrimatives (a :*: b) = gtoNamedEventPrimatives a <> gtoNamedEventPrimatives b

class GToEvent1 f where
  gmakeEventType1 :: f p -> String
  gmakeEventData1 :: f p -> [(BS.ByteString, EventPrimative)]

instance (GToNamedEventPrimatives f, Datatype d) => GToEvent1 (D1 d f) where
  gmakeEventType1 = datatypeName
  gmakeEventData1 (M1 x) = gtoNamedEventPrimatives x

-- | A class representing a type that can be emitted as an event in the
-- | event logs for the deliverTx response.
class ToEvent e where
  makeEventType :: e -> String
  makeEventData :: e -> [(BS.ByteString, EventPrimative)]

  default makeEventData :: (Generic e, GToEvent1 (Rep e)) => e -> [(BS.ByteString, EventPrimative)]
  makeEventData = gmakeEventData1 . from

  default makeEventType :: (Generic e, GToEvent1 (Rep e)) => e -> String
  makeEventType = gmakeEventType1 . from

instance ToEventPrimative Int where
  toEventPrimative = EventInt . toInteger

instance ToEventPrimative String where
  toEventPrimative = EventString . cs

data Test = Test
  {a :: Int, b :: String}
  deriving (Generic)

instance ToEvent Test

makeEvent
  :: ToEvent e
  => e
  -> Event
makeEvent (e :: e) = Event
  { eventType = cs $ makeEventType e
  , eventAttributes = (\(k, v) -> KVPair (Base64.fromBytes k) (encodeEventPrimative v)) <$> makeEventData e
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
    A.object [ "event_type" A..= makeEventType a
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
  Log.log Log.Info (cs $ makeEventType event)
