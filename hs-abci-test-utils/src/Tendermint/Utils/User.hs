module Tendermint.Utils.User where

import           Crypto.Secp256k1                 (CompactRecSig (..), SecKey,
                                                   derivePubKey,
                                                   exportCompactRecSig, secKey)
import qualified Data.ByteArray.HexString         as Hex
import           Data.ByteString                  (ByteString, snoc)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Short            (fromShort)
import           Data.Maybe                       (fromJust)
import           Data.Proxy
import           Data.String                      (fromString)
import           Data.String.Conversions          (cs)
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Crypto            (Secp256k1, addressFromPubKey)
import           Tendermint.SDK.Types.Address     (Address (..))
import           Tendermint.SDK.Types.Transaction (RawTransaction (..),
                                                   signRawTransaction)

data User = User
  { userPrivKey :: SecKey
  , userAddress :: Address
  }

makeUser :: String -> User
makeUser privKeyStr =
  let privateKey = fromJust . secKey . Hex.toBytes . fromString $ privKeyStr
      pubKey = derivePubKey privateKey
      address = addressFromPubKey (Proxy @Secp256k1) pubKey
  in User privateKey address

algProxy :: Proxy Secp256k1
algProxy = Proxy

-- sign a trx with a user's private key
mkSignedRawTransactionWithRoute :: HasCodec a => BS.ByteString -> SecKey -> a -> RawTransaction
mkSignedRawTransactionWithRoute route privateKey msg = sign unsigned
  where unsigned = RawTransaction { rawTransactionData = encode msg
                                  , rawTransactionRoute = cs route
                                  , rawTransactionSignature = ""
                                  }
        sig = signRawTransaction algProxy privateKey unsigned
        sign rt = rt { rawTransactionSignature = encodeCompactRecSig $ exportCompactRecSig sig }

encodeCompactRecSig :: CompactRecSig -> ByteString
encodeCompactRecSig (CompactRecSig r s v) = snoc (fromShort r <> fromShort s) v
