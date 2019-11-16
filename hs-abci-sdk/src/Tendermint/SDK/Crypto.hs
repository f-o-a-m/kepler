module Tendermint.SDK.Crypto
  ( MakeDigest(..)
  , SignatureSchema(..)
  , RecoverableSignatureSchema(..)
  , Secp256k1
  ) where

import           Control.Error                (hush)
import           Crypto.Hash                  (Digest, hashWith)
import           Crypto.Hash.Algorithms       (Keccak_256 (..), SHA256)
import qualified Crypto.Secp256k1             as Secp256k1
import           Data.ByteArray               (convert)
import qualified Data.ByteString              as B
import           Data.Proxy
import qualified Data.Serialize               as Serialize
import           Tendermint.SDK.Types.Address (Address, addressFromBytes)
import Data.Maybe (fromMaybe)

-- | Class encapsulating data which can hashed.
class MakeDigest a where
  makeDigest :: a -> Digest SHA256

-- | Defines the types and methods for the signature schema parameterized by 'alg'.
class SignatureSchema alg where
    type PubKey alg :: *
    type PrivateKey alg :: *
    type Signature alg :: *
    type Message alg :: *

    sign :: Proxy alg -> PrivateKey alg -> Message alg -> Signature alg
    verify :: Proxy alg -> PubKey alg -> Signature alg -> Message alg -> Bool

    makePubKey :: Proxy alg -> B.ByteString -> Maybe (PubKey alg)
    makeSignature :: Proxy alg -> B.ByteString -> Maybe (Signature alg)
    addressFromPubKey :: Proxy alg -> PubKey alg -> Address

-- | Class allowing for signing and recovering signatures for messages.
class SignatureSchema alg => RecoverableSignatureSchema alg where
    type RecoverableSignature alg :: *

    signRecoverableMessage :: Proxy alg -> PrivateKey alg -> Message alg -> RecoverableSignature alg
    recover :: Proxy alg -> RecoverableSignature alg -> Message alg -> Maybe (PubKey alg)

    makeRecoverableSignature :: Proxy alg -> B.ByteString -> Maybe (RecoverableSignature alg)

data Secp256k1

msgFromSHA256 :: Digest SHA256 -> Secp256k1.Msg
msgFromSHA256 dig = fromMaybe (error "Digest SHA256 wasn't 32 bytes.") $ 
  Secp256k1.msg $ convert dig

instance SignatureSchema Secp256k1 where
    type PubKey Secp256k1 = Secp256k1.PubKey
    type PrivateKey Secp256k1 = Secp256k1.SecKey
    type Signature Secp256k1 = Secp256k1.Sig
    type Message Secp256k1 = Digest SHA256

    sign _ priv dig = Secp256k1.signMsg priv (msgFromSHA256 dig)
    verify _ pub sig dig = Secp256k1.verifySig pub sig (msgFromSHA256 dig)

    makePubKey _ = Secp256k1.importPubKey
    makeSignature _ = Secp256k1.importSig
    -- For lack of a better idea, we're just going to use the Ethereum style here
    addressFromPubKey _ = addressFromBytes . B.drop 12 . convert .
      hashWith Keccak_256 . Secp256k1.exportPubKey False

instance RecoverableSignatureSchema Secp256k1 where
    type RecoverableSignature Secp256k1 = Secp256k1.RecSig

    signRecoverableMessage _ priv dig = Secp256k1.signRecMsg priv (msgFromSHA256 dig)
    recover _ sig dig = Secp256k1.recover sig (msgFromSHA256 dig)
    -- NOTE: I think the use of Data.Serialize is harmless here, because it basically
    -- just peels off bytes <https://github.com/haskoin/secp256k1-haskell/blob/master/src/Crypto/Secp256k1/Internal.hs>
    makeRecoverableSignature _ bs = Secp256k1.importCompactRecSig =<< hush (Serialize.decode bs)
