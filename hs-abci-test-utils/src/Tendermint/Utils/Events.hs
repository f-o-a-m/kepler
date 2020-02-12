module Tendermint.Utils.Events where

--import           Control.Error                          (fmapL)
--import qualified Data.Aeson                             as A
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
--import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
--import           GHC.Exts                               (fromList)
import qualified Data.List                              as L
import           GHC.Generics
import           Network.ABCI.Types.Messages.FieldTypes (KVPair (..))
import           Tendermint.SDK.BaseApp.Events          (Event (..), ToEvent)
import           Tendermint.SDK.Codec                   (HasCodec (..))

-- Event ~ {eventType :: String, kvs :: [(ByteString, ByteString)]}

class GFromNamedEventPrimatives f where
  gfromNamedEventPrimatives :: [(BS.ByteString, BS.ByteString)] -> Either Text (f a)

instance (Selector s, HasCodec a) => GFromNamedEventPrimatives (S1 s (K1 i a)) where
  gfromNamedEventPrimatives kvs =
    let name = selName (undefined :: S1 s (K1 i a) p)
    in case L.lookup (cs name) kvs of
         Nothing -> Left $ "Could not find key " <> cs name <> " in Event key-values."
         Just val -> M1 . K1 <$> decode val

instance (GFromNamedEventPrimatives f) => GFromNamedEventPrimatives (C1 c f) where
  gfromNamedEventPrimatives = fmap M1 . gfromNamedEventPrimatives


instance (GFromNamedEventPrimatives a, GFromNamedEventPrimatives b) => GFromNamedEventPrimatives (a :*: b) where
  gfromNamedEventPrimatives kvs =
    (:*:) <$> gfromNamedEventPrimatives kvs <*> gfromNamedEventPrimatives kvs

class GFromEvent1 f where
  gfromEventData1 :: Event -> Either Text (f p)

instance (GFromNamedEventPrimatives f, Datatype d) => GFromEvent1 (D1 d f) where
  gfromEventData1 Event{eventType, eventAttributes} =
    let dt = cs $ datatypeName (undefined ::  D1 d f p)
    in if dt == eventType
         then fmap M1 . gfromNamedEventPrimatives $
                map (\(KVPair k v) -> (Base64.toBytes k, Base64.toBytes v)) eventAttributes
         else Left $ "Expected Event type " <> dt <> " does not match found Event type " <> cs eventType <> "."

class ToEvent e => FromEvent e where
  fromEvent :: Event -> Either Text e

  default fromEvent :: (Generic e, GFromEvent1 (Rep e)) => Event -> Either Text e
  fromEvent = fmap to . gfromEventData1


{-
-- | A class that can parse event log items in the deliverTx response. Primarily
-- | useful for client applications and testing.
class ToEvent e => FromEvent e where
  fromEvent :: Event -> Either Text e

  default fromEvent :: A.FromJSON e => Event -> Either Text e
  fromEvent Event{eventType, eventAttributes} =
    let expectedType = makeEventType (Proxy @e)
    in if cs eventType /= expectedType
         then fail ("Couldn't match expected event type " <> expectedType <>
                " with found type " <> cs eventType)
         else
           let fromKVPair :: KVPair -> Either String (Text, A.Value)
               fromKVPair (KVPair k v) = do
                 value <- A.eitherDecode . cs @BS.ByteString . Base64.toBytes $ v
                 return (cs @BS.ByteString . Base64.toBytes $ k, value)
           in fmapL cs $ do
             kvPairs <- traverse fromKVPair eventAttributes
             A.eitherDecode . A.encode . A.Object . fromList $ kvPairs
-}
