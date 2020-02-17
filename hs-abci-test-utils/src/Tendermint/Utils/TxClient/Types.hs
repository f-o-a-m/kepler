module Tendermint.Utils.TxClient.Types where

import           Control.Lens                           ((^.))
import           Crypto.Hash                            (Digest)
import           Crypto.Hash.Algorithms                 (SHA256)
import           Data.Bifunctor                         (first)
import qualified Data.ByteArray.Base64String            as Base64
import           Data.Int                               (Int64)
import           Data.Proxy
import           Data.Text                              (Text)
import           Network.ABCI.Types.Messages.FieldTypes (Event)
import qualified Network.ABCI.Types.Messages.Response   as Response
import qualified Network.Tendermint.Client              as RPC
import           Tendermint.SDK.BaseApp.Errors          (AppError,
                                                         txResultAppError)
import qualified Tendermint.SDK.BaseApp.Transaction     as T
import           Tendermint.SDK.Codec                   (HasCodec (..))
import           Tendermint.SDK.Crypto                  (RecoverableSignatureSchema (..),
                                                         SignatureSchema (..))
import           Tendermint.SDK.Types.Address           (Address)
import           Tendermint.SDK.Types.Transaction       (RawTransaction (..),
                                                         signRawTransaction)
import           Tendermint.SDK.Types.TxResult          (checkTxTxResult,
                                                         deliverTxTxResult)

data TxOpts = TxOpts
  { txOptsGas    :: Int64
  , txOptsSigner :: Signer
  }

data Signer = Signer
  { signerAddress :: Address
  , signerSign    :: RawTransaction -> RawTransaction
  }

makeSignerFromKey
  :: RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => Proxy alg
  -> PrivateKey alg
  -> Signer
makeSignerFromKey pa privKey = Signer (addressFromPubKey pa . derivePubKey pa $ privKey) $ \r ->
  let sig = serializeRecoverableSignature pa $
         signRawTransaction pa privKey $ r {rawTransactionSignature = ""}
  in r {rawTransactionSignature = sig}

data TxResponse a =
    TxResponse
      { txResponseResult :: a
      , txResponseEvents :: [Event]
      }
  | TxError AppError
  deriving (Eq, Show)

data SynchronousResponse c d = SynchronousResponse
  { checkTxResponse   :: TxResponse c
  , deliverTxResponse :: TxResponse d
  } deriving (Eq, Show)

data TxClientResponse c d =
    RPCError Text
  | ParseError T.RouteContext Text
  | Response (SynchronousResponse c d)
  deriving (Eq, Show)

parseRPCResponse
  :: forall check deliver.
     HasCodec check
  => HasCodec deliver
  => RPC.ResultBroadcastTxCommit
  -> TxClientResponse check deliver
parseRPCResponse RPC.ResultBroadcastTxCommit{..} =
      let
          makeCheckResp r@Response.CheckTx{..} = case checkTxCode of
              0 -> do
                resp <- decode $ Base64.toBytes checkTxData
                pure $ TxResponse resp $ checkTxEvents
              _ -> Right . TxError $ r ^. checkTxTxResult . txResultAppError

          makeDeliverResp r@Response.DeliverTx{..} = case deliverTxCode of
              0 -> do
                resp <- decode $ Base64.toBytes deliverTxData
                pure $ TxResponse resp $ deliverTxEvents
              _ -> Right . TxError $ r ^. deliverTxTxResult . txResultAppError

          eResponses = do
            checkResp <- first (ParseError T.CheckTx) $
              makeCheckResp resultBroadcastTxCommitCheckTx
            deliverResp <- first (ParseError T.DeliverTx) $
              makeDeliverResp resultBroadcastTxCommitDeliverTx
            pure (checkResp, deliverResp)

      in case eResponses of
          Left e -> e
          Right (check, deliver) -> Response $ SynchronousResponse check deliver
