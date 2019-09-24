module Tendermint.SDK.AuthTreeStore where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TVar
import qualified Crypto.Data.Auth.Tree            as AT
import qualified Crypto.Data.Auth.Tree.Class      as AT
import qualified Crypto.Data.Auth.Tree.Cryptonite as Cryptonite
import qualified Crypto.Hash                      as Cryptonite
import           Data.ByteArray                   (convert)
import           Tendermint.SDK.Store
import Control.Monad.IO.Class

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------

newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b


mkAuthTreeStore :: MonadIO m => m (RawStore m)
mkAuthTreeStore = liftIO $ do
  treeV <- newTVarIO AT.empty
  pure $ RawStore
    { rawStorePut = \k v -> liftIO . atomically $ do
        tree <- readTVar treeV
        writeTVar treeV $ AT.insert k v tree
    , rawStoreGet = \_ k -> liftIO . atomically $ do
        tree <- readTVar treeV
        pure $ AT.lookup k tree
    , rawStoreProve = \_ _ -> pure Nothing
    , rawStoreRoot = liftIO . atomically $ do
        tree <- readTVar treeV
        let AuthTreeHash r = AT.merkleHash tree :: AuthTreeHash
        pure $ Root $ convert r
    }