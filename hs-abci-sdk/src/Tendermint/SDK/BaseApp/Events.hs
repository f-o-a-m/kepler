module Tendermint.SDK.BaseApp.Events
  where

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

-- | A class representing a type that can be emitted as an event in the
-- | event logs for the deliverTx response.
class ToEvent e where
  makeEventType :: Proxy e -> String
  makeEventData :: e -> [(BS.ByteString, BS.ByteString)]


  default makeEventData :: (Generic e, ToEvent1 (Rep e)) => e -> [(BS.ByteString, BS.ByteString)]
  makeEventData = defaultMakeEventData

  default makeEventType :: (Generic e, ToEvent1 (Rep e)) => Proxy e -> String
  makeEventType  _ = defaultMakeEventType (undefined :: e)

class ToEvent1 f where
  makeEventType1 :: f p -> String
  makeEventData1 :: f p -> [(BS.ByteString, BS.ByteString)]

-- instance ToEvent1 (a :*: b) where
--   makeEventType1 _ = "Jimmy"
--   makeEventData1 _ =  [("kook", "kook")]

instance ToEvent1 (a :*: b) where
  makeEventType1 _ = undefined
  makeEventData1 _ =  [("kook", "kook")]

instance (ToEvent1 f, Datatype c) => ToEvent1 (M1 i c f) where
  makeEventType1 m = datatypeName m
  makeEventData1 (M1 x) =  makeEventData1 x


defaultMakeEventData :: (Generic e, ToEvent1 (Rep e)) => e -> [(BS.ByteString, BS.ByteString)]
defaultMakeEventData = makeEventData1 . from

defaultMakeEventType :: (Generic e, ToEvent1 (Rep e)) => e -> String
defaultMakeEventType = makeEventType1 . from

data Test = Test
  {a :: Int, b :: String}
  deriving (Generic)

instance ToEvent Test

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
