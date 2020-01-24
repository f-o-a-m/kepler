module Tendermint.Utils.User where

import           Crypto.Secp256k1                 (CompactRecSig (..), SecKey,
                                                   derivePubKey,
                                                   exportCompactRecSig, secKey)
import qualified Data.ByteArray.HexString         as Hex
import           Data.ByteString                  (ByteString, snoc)
import qualified Data.ByteString                  as BS
import           Data.ByteString.Short            (fromShort)
import           Data.Default.Class               (def)
import           Data.Maybe                       (fromJust)
import           Data.Proxy
import           Data.String                      (fromString)
import           Data.String.Conversions          (cs)
import           Data.Word                        (Word64)
import qualified Network.Tendermint.Client        as RPC
import           Servant.API                      ((:>))
import           Tendermint.SDK.BaseApp.Query     (QueryArgs (..),
                                                   defaultQueryWithData)
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Crypto            (Secp256k1, addressFromPubKey)
import           Tendermint.SDK.Modules.Auth      (Account (..))
import qualified Tendermint.SDK.Modules.Auth      as Auth
import           Tendermint.SDK.Types.Address     (Address (..))
import           Tendermint.SDK.Types.Transaction (RawTransaction (..),
                                                   signRawTransaction)
import           Tendermint.Utils.Client          (ClientResponse (..),
                                                   HasClient (..))
import           Tendermint.Utils.Request         (runRPC)

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

getAccount :: QueryArgs Address -> RPC.TendermintM (ClientResponse Account)
getAccount =
  let apiP = Proxy :: Proxy ("auth" :> Auth.Api)
  in genClient (Proxy :: Proxy RPC.TendermintM) apiP def

getAccountNonce :: Address -> IO Word64
getAccountNonce userAddress = do
  let query = getAccount $ defaultQueryWithData userAddress
  ClientResponse{clientResponseData} <- runRPC query
  case clientResponseData of
    -- @NOTE: unitialized account -> txNonce == 1  (i.e., this is the first transaction)
    Nothing                     -> return 1
    Just Account {accountNonce} -> return (accountNonce + 1)

-- sign a trx with a user's private key and add the user's account nonce
mkSignedRawTransactionWithRoute :: HasCodec a => BS.ByteString -> User -> a -> IO RawTransaction
mkSignedRawTransactionWithRoute route User{userAddress, userPrivKey} msg = do
  nonce <- getAccountNonce userAddress
  let unsigned = RawTransaction { rawTransactionData = encode msg
                                , rawTransactionRoute = cs route
                                , rawTransactionSignature = ""
                                , rawTransactionGas = 0
                                , rawTransactionNonce = nonce
                                }
      sig = signRawTransaction algProxy userPrivKey unsigned
      sign rt = rt { rawTransactionSignature = encodeCompactRecSig $ exportCompactRecSig sig }
  return . sign $ unsigned

encodeCompactRecSig :: CompactRecSig -> ByteString
encodeCompactRecSig (CompactRecSig r s v) = snoc (fromShort r <> fromShort s) v
