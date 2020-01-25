module Tendermint.Utils.User where

import           Crypto.Secp256k1                (SecKey, derivePubKey, secKey)
import qualified Data.ByteArray.HexString        as Hex
import           Data.Maybe                      (fromJust)
import           Data.Proxy
import           Data.String                     (fromString)
import           Tendermint.SDK.Crypto           (Secp256k1, addressFromPubKey)
import           Tendermint.SDK.Types.Address    (Address (..))
import           Tendermint.Utils.TxClient.Types (Signer, makeSignerFromKey)

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

makeSignerFromUser
  :: User
  -> Signer
makeSignerFromUser User{userPrivKey} =
  makeSignerFromKey (Proxy @Secp256k1) userPrivKey
