module Tendermint.SDK.AuthTreeStore where

import           Control.Concurrent.STM           (atomically)
import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import qualified Crypto.Data.Auth.Tree            as AT
import qualified Crypto.Data.Auth.Tree.Class      as AT
import qualified Crypto.Data.Auth.Tree.Cryptonite as Cryptonite
import qualified Crypto.Hash                      as Cryptonite
import           Data.ByteArray                   (convert)
import Data.ByteString (ByteString)
import           Tendermint.SDK.Store
import Polysemy
--------------------------------------------------------------------------------
--
--------------------------------------------------------------------------------

newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b


interpretAuthTreeStore
  :: Member (Embed IO) r
  => Sem (RawStore ': r) a
  -> Sem r a
interpretAuthTreeStore m = do
  treeV :: TVar (AT.Tree ByteString ByteString) <- liftIO $ newTVarIO AT.empty
  interpret
    (\case
      RawStorePut k v -> liftIO . atomically $ do
        tree <- readTVar treeV
        writeTVar treeV $ AT.insert k v tree
      RawStoreGet _ k -> liftIO . atomically $ do
        tree <- readTVar treeV
        pure $ AT.lookup k tree
      RawStoreProve _ _ -> pure Nothing
      RawStoreRoot -> liftIO . atomically $ do
        tree <- readTVar treeV
        let AuthTreeHash r = AT.merkleHash tree :: AuthTreeHash
        pure $ Root $ convert r
    ) m
