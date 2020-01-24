{-# LANGUAGE UndecidableInstances #-}

module Tendermint.Utils.TxClient where

import           Control.Lens                                ((^.))
import           Crypto.Hash                                 (Digest)
import           Crypto.Hash.Algorithms                      (SHA256)
import           Data.Bifunctor                              (first)
import qualified Data.ByteArray.Base64String                 as Base64
import           Data.Int                                    (Int64)
import           Data.Proxy
import           Data.String.Conversions                     (cs)
import           Data.Text                                   (Text)
import           Data.Word                                   (Word64)
import           GHC.TypeLits                                (KnownSymbol,
                                                              symbolVal)
import           Network.ABCI.Types.Messages.FieldTypes      (Event)
import qualified Network.ABCI.Types.Messages.Response        as Response
import qualified Network.Tendermint.Client                   as RPC
import           Servant.API                                 ((:<|>) (..), (:>))
import           Tendermint.SDK.BaseApp.Errors               (AppError,
                                                              txResultAppError)
import qualified Tendermint.SDK.BaseApp.Transaction          as T
import qualified Tendermint.SDK.BaseApp.Transaction.Modifier as T
import           Tendermint.SDK.Codec                        (HasCodec (..))
import           Tendermint.SDK.Crypto                       (RecoverableSignatureSchema (..),
                                                              SignatureSchema (..))
import           Tendermint.SDK.Types.Message                (HasMessageType (..),
                                                              TypedMessage (..))
import           Tendermint.SDK.Types.Transaction            (RawTransaction (..),
                                                              signRawTransaction)
import           Tendermint.SDK.Types.TxResult               (checkTxTxResult,
                                                              deliverTxTxResult)

data TxOpts = TxOpts
  { txOptsRoute :: Text
  , txOptsGas   :: Int64
  , txOptsNonce :: Word64
  }

makeRawTxForSigning
  :: forall msg.
     HasMessageType msg
  => HasCodec msg
  => TxOpts
  -> msg
  -> RawTransaction
makeRawTxForSigning TxOpts{..} msg =
  RawTransaction
    { rawTransactionData = TypedMessage (encode msg) (messageType $ Proxy @msg)
    , rawTransactionGas = txOptsGas
    , rawTransactionNonce = txOptsNonce
    , rawTransactionRoute = txOptsRoute
    , rawTransactionSignature = ""
    }

data Signer = Signer (RawTransaction -> RawTransaction)

makeSignerFromKey
  :: RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => Proxy alg
  -> PrivateKey alg
  -> Signer
makeSignerFromKey pa privKey = Signer $ \r ->
  let sig = serializeRecoverableSignature pa $
         signRawTransaction pa privKey $ r {rawTransactionSignature = ""}
  in r {rawTransactionSignature = sig}

class Monad m => RunTxClient m where
    -- | How to make a request.
    runTx :: RawTransaction -> m RPC.ResultBroadcastTxCommit
    getSigner :: m Signer

class HasClient m layout where

    type ClientT (m :: * -> *) layout :: *
    genClient :: Proxy m -> Proxy layout -> TxOpts -> ClientT m layout

instance (HasClient m a, HasClient m b) => HasClient m (a :<|> b) where
    type ClientT m (a :<|> b) = ClientT m a :<|> ClientT m b
    genClient pm _ opts = genClient pm (Proxy @a) opts :<|> genClient pm (Proxy @b) opts

instance (KnownSymbol path, HasClient m a) => HasClient m (path :> a) where
    type ClientT m (path :> a) = ClientT m a
    genClient pm _ opts = genClient pm (Proxy @a) $
      opts { txOptsRoute = cs $ symbolVal (Proxy @path) }

data TxResponse a =
    TxResponse
      { txResponseResult :: a
      , txResponseEvents :: [Event]
      }
  | TxError AppError

data SynchronousResponse c d = SynchronousResponse
  { checkTxResponse   :: TxResponse c
  , deliverTxResponse :: TxResponse d
  }

data ClientResponse c d =
    RPCError Text
  | ParseError T.RouteContext Text
  | Response (SynchronousResponse c d)

parseRPCResponse
  :: HasCodec a
  => HasCodec (T.OnCheckReturn 'T.CheckTx oc a)
  => Proxy a
  -> Proxy (oc :: T.OnCheck)
  -> RPC.ResultBroadcastTxCommit
  -> ClientResponse (T.OnCheckReturn 'T.CheckTx oc a) a
parseRPCResponse _ _ RPC.ResultBroadcastTxCommit{..} =
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


instance ( HasMessageType msg, HasCodec msg
         , HasCodec a, HasCodec (T.OnCheckReturn 'T.CheckTx oc a)
         , RunTxClient m
         ) => HasClient m (T.TypedMessage msg T.:~> T.Return' oc a) where
    type ClientT m (T.TypedMessage msg T.:~> T.Return' oc a) = msg -> m (ClientResponse (T.OnCheckReturn 'T.CheckTx oc a) a)

    genClient _ _ opts msg = do
      let rawTxForSigning = makeRawTxForSigning opts msg
      Signer signer <- getSigner
      let rawTxWithSig = signer rawTxForSigning
      txRes <- runTx rawTxWithSig
      pure $ parseRPCResponse (Proxy @a) (Proxy @oc) txRes
