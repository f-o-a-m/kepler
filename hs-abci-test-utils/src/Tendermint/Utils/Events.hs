{-# OPTIONS_GHC -fno-warn-orphans #-}
module Tendermint.Utils.Events where

import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import qualified Data.List                              as L
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           GHC.Generics
import           Network.ABCI.Types.Messages.FieldTypes (KVPair (..))
import           Tendermint.SDK.BaseApp.Events          (Event (..), ToEvent)
import           Tendermint.SDK.Codec                   (HasCodec (..))


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

class GFromEvent f where
  gfromEventData :: Event -> Either Text (f p)

instance (GFromNamedEventPrimatives f, Datatype d) => GFromEvent (D1 d f) where
  gfromEventData Event{eventType, eventAttributes} =
    let dt = cs $ datatypeName (undefined ::  D1 d f p)
    in if dt == eventType
         then fmap M1 . gfromNamedEventPrimatives $
                map (\(KVPair k v) -> (Base64.toBytes k, Base64.toBytes v)) eventAttributes
         else Left $ "Expected Event type " <> dt <> " does not match found Event type " <> cs eventType <> "."

class ToEvent e => FromEvent e where
  fromEvent :: Event -> Either Text e

  default fromEvent :: (Generic e, GFromEvent (Rep e)) => Event -> Either Text e
  fromEvent = fmap to . gfromEventData

