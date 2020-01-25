module Tendermint.Utils.ClientUtils where

import           Control.Monad                          (unless)
import           Data.Aeson                             (ToJSON)
import           Data.Aeson.Encode.Pretty               (encodePretty)
import           Data.Either                            (partitionEithers)
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           Data.Word                              (Word32)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..))
import qualified Network.ABCI.Types.Messages.Response   as Response
import qualified Network.Tendermint.Client              as RPC
import           Tendermint.SDK.BaseApp.Errors          (AppError (..))
import           Tendermint.SDK.BaseApp.Events          (ToEvent (..))
import           Tendermint.Utils.Client                (QueryClientResponse (..),
                                                         SynchronousResponse (..),
                                                         TxClientResponse (..),
                                                         TxResponse (..))
import           Tendermint.Utils.Events                (FromEvent (..))

--------------------------------------------------------------------------------
-- | Tx helpers
--------------------------------------------------------------------------------

assertTx
 :: Monad m
 => m (TxClientResponse a b)
 -> m (SynchronousResponse a b)
assertTx m = do
  resp <- m
  case resp of
    Response r -> pure r
    RPCError err        -> fail $ "Expected Response, got RPCError " <> show err
    ParseError ctx err -> fail $ "Expected Response, got ParseError in context " <> show ctx
      <> ": " <> show err

-- get the logged events from a deliver response,
deliverTxEvents
  :: FromEvent e
  => Proxy e
  -> SynchronousResponse a b
  -> IO ([Text],[e])
deliverTxEvents pE SynchronousResponse{deliverTxResponse} =
  case deliverTxResponse of
    TxResponse {txResponseEvents} ->
      let eventName = cs $ makeEventType pE
          es = filter ((== eventName) . eventType) txResponseEvents
      in return . partitionEithers . map fromEvent $ es
    TxError appError -> fail (show appError)

-- check for a specific check response code
ensureCheckResponseCode
  :: Word32
  -> SynchronousResponse a b
  -> IO ()
ensureCheckResponseCode code SynchronousResponse{checkTxResponse} =
   case checkTxResponse of
     TxResponse _ _ ->
        unless (code == 0) $
          fail $ "Couldn't match found checkTx response code 0 with expected code " <> show code <> "."
     TxError appError ->
       let errCode = appErrorCode appError
       in unless (errCode == code) $
            fail $ "Couldn't match found checkTx response code " <> show errCode <>
              " with expected code " <> show code <> "."

-- check for a specific check response code
ensureDeliverResponseCode
  :: Word32
  -> SynchronousResponse a b
  -> IO ()
ensureDeliverResponseCode code SynchronousResponse{deliverTxResponse} =
  case deliverTxResponse of
    TxResponse _ _ ->
        unless (code == 0) $
          fail $ "Couldn't match found deliverTx response code 0 with expected code " <> show code <> "."
    TxError appError ->
      let errCode = appErrorCode appError
      in unless (errCode == code) $
           fail $ "Couldn't match found deliverTx response code " <> show errCode <>
             " with expected code " <> show code <> "."

ensureResponseCodes
  :: (Word32, Word32)
  -> SynchronousResponse a b
  -> IO ()
ensureResponseCodes (checkCode, deliverCode) = do
    _ <- ensureCheckResponseCode checkCode
    ensureDeliverResponseCode deliverCode

--------------------------------------------------------------------------------
-- | Query helpers
--------------------------------------------------------------------------------

assertQuery
  :: Monad m
  => m (QueryClientResponse a)
  -> m (a, Response.Query)
assertQuery m = do
  resp <- m
  case resp of
      QueryResponse r q -> pure (r, q)
      QueryError err    -> fail $ show err

--------------------------------------------------------------------------------

rpcConfig :: RPC.Config
rpcConfig =
  let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
      prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
      prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
  in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")
