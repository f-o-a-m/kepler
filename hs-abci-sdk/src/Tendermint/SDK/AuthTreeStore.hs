module Tendermint.SDK.AuthTreeStore where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import qualified Crypto.Data.Auth.Tree            as AT
import qualified Crypto.Data.Auth.Tree.Class      as AT
import qualified Crypto.Data.Auth.Tree.Cryptonite as Cryptonite
import qualified Crypto.Hash                      as Cryptonite
import           Data.ByteString                  (ByteString)
import           Polysemy                         (Embed, Member, Sem,
                                                   interpret)
import           Tendermint.SDK.Store             (RawStore (..), StoreKey (..))
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b

data AuthTreeDriver = AuthTreeDriver
  { treeVar :: TVar (AT.Tree ByteString ByteString)
  }

initAuthTreeDriver :: IO AuthTreeDriver
initAuthTreeDriver = AuthTreeDriver <$> newTVarIO AT.empty

interpretAuthTreeStore
  :: Member (Embed IO) r
  => AuthTreeDriver
  -> Sem (RawStore ': r) a
  -> Sem r a
interpretAuthTreeStore AuthTreeDriver{treeVar} =
  interpret
    (\case
      RawStorePut (StoreKey sk) k v -> liftIO . atomically $ do
        tree <- readTVar treeVar
        writeTVar treeVar $ AT.insert (sk <> k) v tree
      RawStoreGet (StoreKey sk) k -> liftIO . atomically $ do
        tree <- readTVar treeVar
        pure $ AT.lookup (sk <> k) tree
      RawStoreProve _ _ -> pure Nothing
      RawStoreDelete (StoreKey sk) k -> liftIO . atomically $ do
        tree <- readTVar treeVar
        writeTVar treeVar $ AT.delete (sk <> k) tree
    )
