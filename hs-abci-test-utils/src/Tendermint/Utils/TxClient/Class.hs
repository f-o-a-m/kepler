{-# LANGUAGE UndecidableInstances #-}

module Tendermint.Utils.TxClient.Class
  ( ClientConfig(..)
  , RunTxClient(..)
  , HasTxClient(..)
  ) where

import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Reader                        (ReaderT, ask)
import qualified Data.ByteArray.Base64String                 as Base64
import           Data.Proxy
import           Data.String.Conversions                     (cs)
import           Data.Word                                   (Word64)
import           GHC.TypeLits                                (KnownSymbol,
                                                              symbolVal)
import qualified Network.Tendermint.Client                   as RPC
import           Servant.API                                 ((:<|>) (..), (:>))
import qualified Tendermint.SDK.BaseApp.Transaction          as T
import qualified Tendermint.SDK.BaseApp.Transaction.Modifier as T
import           Tendermint.SDK.Codec                        (HasCodec (..))
import           Tendermint.SDK.Types.Address                (Address)
import           Tendermint.SDK.Types.Message                (HasMessageType (..))
import           Tendermint.SDK.Types.Transaction            (RawTransaction (..))
import           Tendermint.Utils.TxClient.Types

class Monad m => RunTxClient m where
    -- | How to make a request.
    runTx :: RawTransaction -> m RPC.ResultBroadcastTxCommit
    getNonce :: m Word64
    getSigner :: m Signer

data ClientConfig = ClientConfig
  { clientSigner   :: Signer
  , clientRPC      :: RPC.Config
  , clientGetNonce :: Address -> IO Word64
  }

instance RunTxClient (ReaderT ClientConfig IO) where
    getSigner = clientSigner <$> ask
    getNonce = do
        nonceGetter <- clientGetNonce <$> ask
        Signer address _ <- getSigner
        liftIO $ nonceGetter address
    runTx tx = do
      let txReq = RPC.broadcastTxCommit . RPC.RequestBroadcastTxCommit . Base64.fromBytes . encode $ tx
      rpc <- clientRPC <$> ask
      liftIO . RPC.runTendermintM rpc $ txReq

class HasTxClient m layout where

    type ClientT (m :: * -> *) layout :: *
    genClientT :: Proxy m -> Proxy layout -> TxOpts -> ClientT m layout

instance (HasTxClient m a, HasTxClient m b) => HasTxClient m (a :<|> b) where
    type ClientT m (a :<|> b) = ClientT m a :<|> ClientT m b
    genClientT pm _ opts = genClientT pm (Proxy @a) opts :<|> genClientT pm (Proxy @b) opts

instance (KnownSymbol path, HasTxClient m a) => HasTxClient m (path :> a) where
    type ClientT m (path :> a) = ClientT m a
    genClientT pm _ opts = genClientT pm (Proxy @a) $
      opts { txOptsRoute = cs $ symbolVal (Proxy @path) }


instance ( HasMessageType msg, HasCodec msg
         , HasCodec a, HasCodec (T.OnCheckReturn 'T.CheckTx oc a)
         , RunTxClient m
         ) => HasTxClient m (T.TypedMessage msg T.:~> T.Return' oc a) where
    type ClientT m (T.TypedMessage msg T.:~> T.Return' oc a) = msg -> m (TxClientResponse (T.OnCheckReturn 'T.CheckTx oc a) a)

    genClientT _ _ opts msg = do
      Signer _ signer <- getSigner
      nonce <- getNonce
      let rawTxForSigning = makeRawTxForSigning (opts {txOptsNonce = nonce}) msg
          rawTxWithSig = signer rawTxForSigning
      txRes <- runTx rawTxWithSig
      pure $ parseRPCResponse (Proxy @a) (Proxy @oc) txRes
