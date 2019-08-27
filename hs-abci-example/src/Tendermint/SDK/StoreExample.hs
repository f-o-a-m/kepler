module Tendermint.SDK.StoreExample where

import Control.Lens (iso)
import Data.Binary (Binary, encode, decode)
import Data.ByteArray (convert)
import Tendermint.SDK.Store
import Tendermint.SDK.Codec
import Data.String.Conversions (cs)
import GHC.Generics (Generic)
import qualified Crypto.Data.Auth.Tree       as AT
import qualified Crypto.Data.Auth.Tree.Class as AT
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM (atomically)
import qualified Crypto.Hash as Cryptonite
import qualified Crypto.Data.Auth.Tree.Cryptonite as Cryptonite



--------------------------------------------------------------------------------
-- Example Store
--------------------------------------------------------------------------------

newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b

type AuthTreeRawStore = RawStore IO

mkAuthTreeStore :: IO AuthTreeRawStore
mkAuthTreeStore = do
  treeV <- newTVarIO AT.empty
  pure $ RawStore
    { rawStorePut = \k v -> atomically $ do 
        tree <- readTVar treeV
        writeTVar treeV $ AT.insert k v tree
    , rawStoreGet = \k -> atomically $ do
        tree <- readTVar treeV
        pure $ AT.lookup k tree
    , rawStoreRoot = atomically $ do
        tree <- readTVar treeV
        let AuthTreeHash r = AT.merkleHash tree :: AuthTreeHash
        pure $ convert r
    }

data User = User 
  { userAddress :: String
  , userName :: String
  } deriving Generic

newtype UserKey = UserKey String

instance Binary User

userCodec :: Codec User
userCodec = 
    Codec { codecEncode = cs . encode
          , codecDecode = decode . cs 
          }

instance HasKey User where
    type Key User = UserKey
    rawKey = iso (\(UserKey k) -> cs k) (UserKey . cs)

type UserStore = Store '[User] IO

putUser
  :: User
  -> UserStore
  -> IO ()
putUser user@User{userAddress} store = put (UserKey userAddress) user store