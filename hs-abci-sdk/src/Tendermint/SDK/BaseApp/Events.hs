module Tendermint.SDK.BaseApp.Events
  ( Event(..)
  , ToEvent(..)
  , FromEvent(..)
  , emit
  , makeEvent
  ) where

import           Control.Error                          (fmapL)
import qualified Data.Aeson                             as A
import           Data.Bifunctor                         (bimap)
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           GHC.Exts                               (fromList, toList)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..),
                                                         KVPair (..))
import           Polysemy                               (Member, Sem)
import           Polysemy.Output                        (Output, output)

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

-- data LoggableEvent a = LoggableEvent a
-- logEvent ::

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
