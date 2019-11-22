module Tendermint.SDK.Types.Transaction where

import           Control.Error                (note)
import           Control.Lens                 (Wrapped (..), from, iso, view,
                                               (&), (.~), (^.))
import           Crypto.Hash                  (Digest, hashWith)
import           Crypto.Hash.Algorithms       (SHA256 (..))
import           Data.Bifunctor               (bimap)
import           Data.ByteString              (ByteString)
import qualified Data.ProtoLens               as P
import           Data.Proxy
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import qualified Proto.Types.Transaction            as T
import qualified Proto.Types.Transaction_Fields     as T
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Crypto        (MakeDigest (..),
                                               RecoverableSignatureSchema (..),
                                               SignatureSchema (..))
import           Tendermint.SDK.Types.Message (Msg (..))

-- Our standard transaction type parameterized by the signature schema 'alg'
-- and an underlying message type 'msg'.
data Tx alg msg = Tx
  { txMsg       :: Msg msg
  , txRoute     :: Text
  , txSignature :: RecoverableSignature alg
  , txSignBytes :: Message alg
  , txSigner    :: PubKey alg
  }

--------------------------------------------------------------------------------

-- TODO: figure out what the actual standards are for these things, if there
-- even are any.

-- | Raw transaction type coming in over the wire
data RawTransaction = RawTransaction
  { rawTransactionData      :: ByteString
  -- ^ the encoded message via protobuf encoding
  , rawTransactionRoute     :: Text
  -- ^ module name
  , rawTransactionSignature :: ByteString
  } deriving Generic

instance Wrapped RawTransaction where
  type Unwrapped RawTransaction = T.RawTransaction

  _Wrapped' = iso t f
   where
    t RawTransaction {..} =
      P.defMessage
        & T.data' .~ rawTransactionData
        & T.route .~ cs rawTransactionRoute
        & T.signature .~ rawTransactionSignature
    f message = RawTransaction
      { rawTransactionData      = message ^. T.data'
      , rawTransactionRoute = message ^. T.route
      , rawTransactionSignature = message ^. T.signature
      }

instance HasCodec RawTransaction where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance MakeDigest RawTransaction where
  makeDigest tx = hashWith SHA256 . encode $ tx {rawTransactionSignature = ""}

signRawTransaction
  :: forall alg.
     RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => Proxy alg
  -> PrivateKey alg
  -> RawTransaction
  -> RecoverableSignature alg
signRawTransaction p priv tx = signRecoverableMessage p priv (makeDigest tx)

-- | Attempt to parse a 'RawTransaction' as a 'Tx' without attempting
-- | to parse the underlying message. This is done as a preprocessing
-- | step to the router, allowing for failure before the router is ever
-- | reached.
parseTx
  :: forall alg.
     RecoverableSignatureSchema alg
  => Message alg ~ Digest SHA256
  => Proxy alg
  -> RawTransaction
  -> Either Text (Tx alg ByteString)
parseTx p rawTx@RawTransaction{..} = do
  recSig <- note "Unable to parse transaction signature as a recovery signature." $
       makeRecoverableSignature p rawTransactionSignature
  let txForSigning = rawTx {rawTransactionSignature = ""}
      signBytes = makeDigest txForSigning
  signerPubKey <- note "Signature recovery failed." $ recover p recSig signBytes
  return Tx
    { txMsg = Msg
      { msgData = rawTransactionData
      , msgAuthor = addressFromPubKey p signerPubKey
      }
    , txRoute = cs rawTransactionRoute
    , txSignature = recSig
    , txSignBytes = signBytes
    , txSigner = signerPubKey
    }

data RoutedTx msg where
  RoutedTx :: Tx alg msg -> RoutedTx msg

