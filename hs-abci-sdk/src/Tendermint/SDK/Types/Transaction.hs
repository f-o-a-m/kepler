module Tendermint.SDK.Types.Transaction where

import qualified Crypto.Secp256k1             as Crypto
import           Data.ByteString              (ByteString)
import           Tendermint.SDK.Types.Message (Msg)

data Tx msg = Tx
  { txMsg       :: Msg msg
  , txSignature :: Crypto.RecSig
  , txSignBytes :: Crypto.Msg
  , txSigner    :: Crypto.PubKey
  }

--------------------------------------------------------------------------------

data Transaction = Transaction
  { transactionData      :: ByteString
  -- ^ the encoded message via protobuf encoding
  , transactionSignature :: ByteString
  -- ^ signature (encoded via Data.Serialize.encode $ (sig :: Crypto.Secp256k1.CompactRecoverySignature)
  , transactionRoute     :: ByteString
  -- ^ module name
  }
