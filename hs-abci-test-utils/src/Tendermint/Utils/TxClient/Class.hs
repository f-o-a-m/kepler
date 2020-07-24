{-# LANGUAGE UndecidableInstances #-}

module Tendermint.Utils.TxClient.Class
  ( ClientConfig(..)
  , RunTxClient(..)
  , HasTxClient(..)
  , EmptyTxClient(..)
  , defaultClientTxOpts
  ) where

import Data.Kind (Type)
import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (ReaderT, ask)
import qualified Data.ByteArray.Base64String        as Base64
import           Data.Proxy
import           Data.String.Conversions            (cs)
import           Data.Text                          (Text)
import           Data.Word                          (Word64)
import           GHC.TypeLits                       (KnownSymbol, symbolVal)
import qualified Network.Tendermint.Client          as RPC
import           Servant.API                        ((:<|>) (..), (:>))
import qualified Tendermint.SDK.BaseApp.Transaction as T
import           Tendermint.SDK.Codec               (HasCodec (..))
import           Tendermint.SDK.Types.Address       (Address)
import           Tendermint.SDK.Types.Message       (HasMessageType (..),
                                                     TypedMessage (..))
import           Tendermint.SDK.Types.Transaction   (RawTransaction (..))
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

class HasTxClient m layoutC layoutD where

    type ClientT (m :: Type -> Type) layoutC layoutD :: Type
    genClientT :: Proxy m -> Proxy layoutC -> Proxy layoutD -> ClientTxOpts -> ClientT m layoutC layoutD

instance (HasTxClient m a c, HasTxClient m b d) => HasTxClient m (a :<|> b) (c :<|> d) where
    type ClientT m (a :<|> b) (c :<|> d) = ClientT m a c :<|> ClientT m b d
    genClientT pm _ _ opts = genClientT pm (Proxy @a) (Proxy @c) opts :<|>
                           genClientT pm (Proxy @b) (Proxy @d) opts

instance (KnownSymbol path, HasTxClient m a b) => HasTxClient m (path :> a) (path :> b) where
    type ClientT m (path :> a) (path :> b) = ClientT m a b
    genClientT pm _ _ clientOpts =
      let clientOpts' = clientOpts { clientTxOptsRoute = cs $ symbolVal (Proxy @path) }
      in  genClientT pm (Proxy @a) (Proxy @b) clientOpts'

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
         , HasCodec check, HasCodec deliver
         , RunTxClient m
         ) => HasTxClient m (T.TypedMessage msg T.:~> T.Return check) (T.TypedMessage msg T.:~> T.Return deliver) where
    type ClientT m (T.TypedMessage msg T.:~> T.Return check) (T.TypedMessage msg T.:~> T.Return deliver) =
           TxOpts -> msg -> m (TxClientResponse check deliver)

    genClientT _ _ _ clientOpts opts msg = do
      let Signer signerAddress signer = txOptsSigner opts
      nonce <- getNonce signerAddress
      let clientOpts' = clientOpts {clientTxOptsNonce = nonce}
          rawTxForSigning = makeRawTxForSigning clientOpts' opts msg
          rawTxWithSig = signer rawTxForSigning
      txRes <- runTx rawTxWithSig
      pure $ parseRPCResponse txRes


-- | Singleton type representing a client for an empty API.
data EmptyTxClient = EmptyTxClient deriving (Eq, Show, Bounded, Enum)

instance HasTxClient m T.EmptyTxServer T.EmptyTxServer where
  type ClientT m T.EmptyTxServer T.EmptyTxServer = EmptyTxClient

  genClientT _ _ _ _  = EmptyTxClient
