module Tendermint.SDK.Types.Transaction where

import           Control.Error                (note)
import           Crypto.Hash                  (Digest, hashWith)
import           Crypto.Hash.Algorithms       (SHA256 (..))
import           Data.ByteString              (ByteString)
import           Data.Proxy
import qualified Data.Serialize               as Serialize
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import           Tendermint.SDK.Crypto        (MakeDigest (..),
                                               RecoverableSignatureSchema (..),
                                               SignatureSchema (..))
import           Tendermint.SDK.Types.Message (Msg (..))

-- Our standard transaction type parameterized by the signature schema 'alg'
-- and an underlying message type 'msg'.
data Tx alg msg = Tx
  { txMsg       :: Msg msg
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
  , rawTransactionRoute     :: ByteString
  -- ^ module name
  , rawTransactionSignature :: ByteString
  } deriving Generic

instance Serialize.Serialize RawTransaction

instance MakeDigest RawTransaction where
  makeDigest tx = hashWith SHA256 . Serialize.encode $ tx {rawTransactionSignature = ""}

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
    , txSignature = recSig
    , txSignBytes = signBytes
    , txSigner = signerPubKey
    }
