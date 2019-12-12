module Tendermint.Utils.Events where

import           Control.Error                          (fmapL)
import qualified Data.Aeson                             as A
import qualified Data.ByteArray.Base64String            as Base64
import qualified Data.ByteString                        as BS
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           GHC.Exts                               (fromList)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..),
                                                         KVPair (..))
import           Tendermint.SDK.BaseApp.Events          (ToEvent, makeEventType)

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
