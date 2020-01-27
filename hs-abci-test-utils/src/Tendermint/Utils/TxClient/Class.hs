{-# LANGUAGE UndecidableInstances #-}

module Tendermint.Utils.TxClient.Class
  ( ClientConfig(..)
  , RunTxClient(..)
  , HasTxClient(..)
  , EmptyTxClient(..)
  , defaultClientTxOpts
  ) where

import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Reader                        (ReaderT, ask)
import qualified Data.ByteArray.Base64String                 as Base64
import           Data.Proxy
import           Data.String.Conversions                     (cs)
import           Data.Text                                   (Text)
import           Data.Word                                   (Word64)
import           GHC.TypeLits                                (KnownSymbol,
                                                              symbolVal)
import qualified Network.Tendermint.Client                   as RPC
import           Servant.API                                 ((:<|>) (..), (:>))
import qualified Tendermint.SDK.BaseApp.Transaction          as T
import qualified Tendermint.SDK.BaseApp.Transaction.Modifier as T
import           Tendermint.SDK.Codec                        (HasCodec (..))
import           Tendermint.SDK.Types.Address                (Address)
import           Tendermint.SDK.Types.Message                (HasMessageType (..),
                                                              TypedMessage (..))
import           Tendermint.SDK.Types.Transaction            (RawTransaction (..))
import           Tendermint.Utils.TxClient.Types

class Monad m => RunTxClient m where
    -- | How to make a request.
    runTx :: RawTransaction -> m RPC.ResultBroadcastTxCommit
    getNonce :: Address -> m Word64

data ClientConfig = ClientConfig
  { clientRPC      :: RPC.Config
  , clientGetNonce :: Address -> IO Word64
  }


instance RunTxClient (ReaderT ClientConfig IO) where
    getNonce addr = do
      nonceGetter <- clientGetNonce <$> ask
      liftIO $ nonceGetter addr
    runTx tx = do
      let txReq = RPC.broadcastTxCommit . RPC.RequestBroadcastTxCommit . Base64.fromBytes . encode $ tx
      rpc <- clientRPC <$> ask
      liftIO . RPC.runTendermintM rpc $ txReq

data ClientTxOpts = ClientTxOpts
  { clientTxOptsRoute :: Text
  , clientTxOptsNonce :: Word64
  }

defaultClientTxOpts :: ClientTxOpts
defaultClientTxOpts = ClientTxOpts "" 0

class HasTxClient m layout where

    type ClientT (m :: * -> *) layout :: *
    genClientT :: Proxy m -> Proxy layout -> ClientTxOpts -> ClientT m layout

instance (HasTxClient m a, HasTxClient m b) => HasTxClient m (a :<|> b) where
    type ClientT m (a :<|> b) = ClientT m a :<|> ClientT m b
    genClientT pm _ opts = genClientT pm (Proxy @a) opts :<|> genClientT pm (Proxy @b) opts

instance (KnownSymbol path, HasTxClient m a) => HasTxClient m (path :> a) where
    type ClientT m (path :> a) = ClientT m a
    genClientT pm _ clientOpts =
      let clientOpts' = clientOpts { clientTxOptsRoute = cs $ symbolVal (Proxy @path) }
      in  genClientT pm (Proxy @a) clientOpts'

makeRawTxForSigning
  :: forall msg.
     HasMessageType msg
  => HasCodec msg
  => ClientTxOpts
  -> TxOpts
  -> msg
  -> RawTransaction
makeRawTxForSigning ClientTxOpts{..} TxOpts{..} msg =
  RawTransaction
    { rawTransactionData = TypedMessage (encode msg) (messageType $ Proxy @msg)
    , rawTransactionGas = txOptsGas
    , rawTransactionNonce = clientTxOptsNonce
    , rawTransactionRoute = clientTxOptsRoute
    , rawTransactionSignature = ""
    }

instance ( HasMessageType msg, HasCodec msg
         , HasCodec a, HasCodec (T.OnCheckReturn 'T.CheckTx oc a)
         , RunTxClient m
         ) => HasTxClient m (T.TypedMessage msg T.:~> T.Return' oc a) where
    type ClientT m (T.TypedMessage msg T.:~> T.Return' oc a) = TxOpts -> msg -> m (TxClientResponse (T.OnCheckReturn 'T.CheckTx oc a) a)

    genClientT _ _ clientOpts opts msg = do
      let Signer signerAddress signer = txOptsSigner opts
      nonce <- getNonce signerAddress
      let clientOpts' = clientOpts {clientTxOptsNonce = nonce}
          rawTxForSigning = makeRawTxForSigning clientOpts' opts msg
          rawTxWithSig = signer rawTxForSigning
      txRes <- runTx rawTxWithSig
      pure $ parseRPCResponse (Proxy @a) (Proxy @oc) txRes


-- | Singleton type representing a client for an empty API.
data EmptyTxClient = EmptyTxClient deriving (Eq, Show, Bounded, Enum)

instance HasTxClient m T.EmptyTxServer where
  type ClientT m T.EmptyTxServer = EmptyTxClient

  genClientT _ _ _ = EmptyTxClient
