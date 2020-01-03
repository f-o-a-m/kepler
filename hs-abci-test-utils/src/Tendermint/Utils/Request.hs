module Tendermint.Utils.Request where

import           Control.Lens                         ((^.))
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import qualified Data.ByteArray.Base64String          as Base64
import           Data.Maybe                           (fromJust)
import           Data.String.Conversions              (cs)
import           Data.Word                            (Word32)
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Tendermint.SDK.Codec                 (HasCodec (..))
import           Tendermint.SDK.Types.Transaction     (RawTransaction (..))
import           Tendermint.Utils.Client              (ClientResponse (..))
import           Test.Hspec

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")

-- executes a query and ensures a 0 response code
getQueryResponseSuccess :: RPC.TendermintM (ClientResponse a) -> IO a
getQueryResponseSuccess query = do
  ClientResponse{clientResponseData,clientResponseRaw} <- runRPC query
  let responseCode = clientResponseRaw ^. Response._queryCode
  responseCode `shouldBe` 0
  return . fromJust $ clientResponseData

-- executes a request, then returns the checkTx response
getCheckTxResponse :: RawTransaction -> IO Response.CheckTx
getCheckTxResponse rawTx = do
  let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  fmap RPC.resultBroadcastTxCommitCheckTx . runRPC $
    RPC.broadcastTxCommit txReq

-- executes a request, then returns the deliverTx response
getDeliverTxResponse :: RawTransaction -> IO Response.DeliverTx
getDeliverTxResponse rawTx = do
  let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $
    RPC.broadcastTxCommit txReq

-- executes a request, check deliver and response codes
ensureCheckAndDeliverResponseCodes :: (Word32, Word32) -> RawTransaction -> IO ()
ensureCheckAndDeliverResponseCodes codes rawTx = do
  let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeRawTx rawTx }
  resp <- runRPC $ RPC.broadcastTxCommit txReq
  let checkResp = RPC.resultBroadcastTxCommitCheckTx resp
      deliverResp = RPC.resultBroadcastTxCommitDeliverTx resp
  codes `shouldBe` (checkResp ^. Response._checkTxCode, deliverResp ^. Response._deliverTxCode)

encodeRawTx :: RawTransaction -> Base64.Base64String
encodeRawTx = Base64.fromBytes . encode
