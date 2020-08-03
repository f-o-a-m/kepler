module Tendermint.Utils.ClientUtils where

import           Control.Monad                          (unless)
import           Data.Aeson                             (ToJSON)
import           Data.Aeson.Encode.Pretty               (encodePretty)
import           Data.Proxy
import           Data.String.Conversions                (cs)
import           Data.Word                              (Word32)
import           Network.ABCI.Types.Messages.FieldTypes (Event (..))
import qualified Network.Tendermint.Client              as RPC
import           Tendermint.SDK.BaseApp.Errors          (AppError (..))
import           Tendermint.SDK.BaseApp.Query           (QueryResult (..))
import           Tendermint.Utils.Client                (QueryClientResponse (..),
                                                         SynchronousResponse (..),
                                                         TxClientResponse (..),
                                                         TxResponse (..))

--------------------------------------------------------------------------------
-- | Tx helpers
--------------------------------------------------------------------------------

assertTx
 :: Monad m
 => MonadFail m
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
  :: Monad m
  => MonadFail m
  => Proxy e
  -> SynchronousResponse a b
  -> m [Event]
deliverTxEvents _ SynchronousResponse{deliverTxResponse} =
  case deliverTxResponse of
    TxResponse {txResponseEvents} -> do
      pure txResponseEvents
    TxError appError -> fail (show appError)

-- check for a specific check response code
ensureCheckResponseCode
  :: MonadFail m
  => Word32
  -> SynchronousResponse a b
  -> m ()
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
  :: Monad m
  => MonadFail m
  => Word32
  -> SynchronousResponse a b
  -> m ()
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
  :: MonadFail m
  => (Word32, Word32)
  -> SynchronousResponse a b
  -> m ()
ensureResponseCodes (checkCode, deliverCode) resp = do
    ensureCheckResponseCode checkCode resp
    ensureDeliverResponseCode deliverCode resp

--------------------------------------------------------------------------------
-- | Query helpers
--------------------------------------------------------------------------------

assertQuery
  :: Monad m
  => MonadFail m
  => m (QueryClientResponse a)
  -> m (QueryResult a)
assertQuery m = do
  resp <- m
  case resp of
      QueryResponse r -> pure r
      QueryError err  -> fail $ show err

ensureQueryResponseCode
  :: Monad m
  => MonadFail m
  => Word32
  -> QueryClientResponse a
  -> m ()
ensureQueryResponseCode code resp = case resp of
  QueryResponse _ ->
    unless (code == 0) $
      fail $ "Couldn't match found query response code 0 with expected code " <> show code <> "."
  QueryError AppError{appErrorCode} ->
    unless (appErrorCode == code) $
      fail $ "Couldn't match found query response code " <> show appErrorCode <>
        " with expected code " <> show code <> "."

--------------------------------------------------------------------------------

rpcConfig :: RPC.Config
rpcConfig =
  let RPC.Config baseReq _ _ host port tls = RPC.defaultConfig "localhost" 26657 False
      prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
      prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
  in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response") host port tls
