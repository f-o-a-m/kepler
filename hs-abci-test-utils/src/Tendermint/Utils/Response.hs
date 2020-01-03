module Tendermint.Utils.Response where

import           Control.Lens                           ((^.))
import           Data.Either                            (partitionEithers)
import           Data.Text                              (Text)
import           Data.Word                              (Word32)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..))
import qualified Network.ABCI.Types.Messages.Response   as Response
import           Tendermint.Utils.Events                (FromEvent (..))
import           Test.Hspec

-- get the logged events from a deliver response,
deliverTxEvents :: FromEvent e => Response.DeliverTx -> Text -> IO ([Text],[e])
deliverTxEvents deliverResp eventName = do
  let deliverEvents = deliverResp ^. Response._deliverTxEvents
      filtered = filter ((== eventName) . eventType) deliverEvents
  return . partitionEithers . map fromEvent $ filtered

-- ensures there are no errors when parsing event logs and contains the expectedEvent
ensureEventLogged :: (Eq e, Show e, FromEvent e) => Response.DeliverTx -> Text -> e -> IO ()
ensureEventLogged deliverResp eventName expectedEvent = do
  (errs, events) <- deliverTxEvents deliverResp eventName
  errs `shouldBe` mempty
  events `shouldSatisfy` elem expectedEvent

-- check for a specific check response code
ensureCheckResponseCode :: Response.CheckTx -> Word32 -> IO ()
ensureCheckResponseCode checkResp code = do
  let checkRespCode = checkResp ^. Response._checkTxCode
  checkRespCode `shouldBe` code

-- check for a specific deliver response code
ensureDeliverResponseCode :: Response.DeliverTx -> Word32 -> IO ()
ensureDeliverResponseCode deliverResp code = do
  let deliverRespCode = deliverResp ^. Response._deliverTxCode
  deliverRespCode `shouldBe` code
