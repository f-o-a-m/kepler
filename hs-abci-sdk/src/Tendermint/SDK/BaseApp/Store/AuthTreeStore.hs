module Tendermint.SDK.BaseApp.Store.AuthTreeStore
  ( AuthTreeState(..)
  , AuthTreeGetter(..)
  , initAuthTreeState
  , evalMergeScopes
  , evalTagged
  ) where

import           Control.Monad                         (forM_)
import           Control.Monad.IO.Class
import qualified Crypto.Data.Auth.Tree                 as AT
import qualified Crypto.Data.Auth.Tree.Class           as AT
import qualified Crypto.Data.Auth.Tree.Cryptonite      as Cryptonite
import qualified Crypto.Hash                           as Cryptonite
import           Data.ByteArray                        (convert)
import           Data.ByteString                       (ByteString)
import           Data.IORef                            (IORef, newIORef,
                                                        readIORef, writeIORef)
import           Data.Map                              (Map)
import qualified Data.Map                              as Map
import           Data.Maybe                            (fromMaybe)
import           Data.Proxy
import           Numeric.Natural                       (Natural)
import           Polysemy                              (Embed, Members, Sem,
                                                        interpret)
import           Polysemy.Error                        (Error)
import           Polysemy.Reader                       (Reader, ask, asks)
import           Polysemy.Tagged                       (Tagged (..))
import           Tendermint.SDK.BaseApp.Errors         (AppError, SDKError (..),
                                                        throwSDKError)
import           Tendermint.SDK.BaseApp.Store.RawStore (RawStore (..),
                                                        makeRawKey)
import           Tendermint.SDK.BaseApp.Store.Scope    (ConnectionScope (..),
                                                        MergeScopes (..),
                                                        Version (..))

-- At the moment, the 'AuthTreeStore' is our only interpreter for the 'RawStore' effect.
-- It is an in memory merklized key value store. You can find the repository here
-- https://github.com/oscoin/avl-auth

newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b

data AuthTree (c :: ConnectionScope) = AuthTree
  { treesVar :: IORef (AT.Tree ByteString ByteString, Map Natural (AT.Tree ByteString ByteString))
  , versionVar :: IORef Version
  }

initAuthTree :: IORef (AT.Tree ByteString ByteString, Map Natural (AT.Tree ByteString ByteString)) -> Version -> IO (AuthTree c)
initAuthTree trees v = AuthTree trees <$> newIORef v

data AuthTreeState = AuthTreeState
  { query     :: AuthTree 'Query
  , mempool   :: AuthTree 'Mempool
  , consensus :: AuthTree 'Consensus
  }

initAuthTreeState :: IO AuthTreeState
initAuthTreeState = do
  treeMap <- newIORef (AT.empty, mempty)
  AuthTreeState <$> initAuthTree treeMap Genesis <*> initAuthTree treeMap Genesis  <*> initAuthTree treeMap Latest

class AuthTreeGetter (s :: ConnectionScope) where
  getAuthTree :: Proxy s -> AuthTreeState -> AuthTree s

instance AuthTreeGetter 'Query where
  getAuthTree _ = query

instance AuthTreeGetter 'Mempool where
  getAuthTree _ = mempool

instance AuthTreeGetter 'Consensus where
  getAuthTree _ = consensus

evalTagged
  :: forall (s :: ConnectionScope) r.
     Members [Reader AuthTreeState, Error AppError, Embed IO] r
  => AuthTreeGetter s
  => forall a. Sem (Tagged s RawStore ': r) a -> Sem r a
evalTagged m = do
  AuthTree{treesVar, versionVar} <- asks (getAuthTree (Proxy :: Proxy s))
  interpret
    (\(Tagged action) -> case action of
      RawStorePut k v -> do
        (headTree, treeMap) <- liftIO $ readIORef treesVar
        version <- liftIO $ readIORef versionVar
        case version of
          Latest ->
            let newHead = AT.insert (makeRawKey k) v headTree
            in liftIO $ writeIORef treesVar (newHead, treeMap)
          _ -> throwSDKError $ RawStoreInvalidOperation "Put"
      RawStoreGet k -> do
        (headTree, treeMap) <- liftIO $ readIORef treesVar
        version <- liftIO $ readIORef versionVar
        case version of
          Latest -> pure $ AT.lookup (makeRawKey k) headTree
          Genesis -> pure $ AT.lookup (makeRawKey k) headTree
          Version v -> pure $ do
            tree <- treeMap Map.!? v
            AT.lookup (makeRawKey k) tree
      RawStoreProve _ -> pure Nothing
      RawStoreDelete k -> do
        (headTree, treeMap) <- liftIO $ readIORef treesVar
        version <- liftIO $ readIORef versionVar
        case version of
          Latest ->
            let newHead = AT.delete (makeRawKey k) headTree
            in liftIO $ writeIORef treesVar (newHead, treeMap)
          _ -> throwSDKError $ RawStoreInvalidOperation "Delete"
      RawStoreRoot -> do
        (headTree, _) <- liftIO $ readIORef treesVar
        version <- liftIO $ readIORef versionVar
        case version of
          Latest -> do
            let AuthTreeHash hash = AT.merkleHash headTree
            pure $ convert hash
          Genesis -> do
            let AuthTreeHash hash = AT.merkleHash headTree
            pure $ convert hash
          Version _ -> throwSDKError $ RawStoreInvalidOperation "Root"
      -- TODO :: Can probably remove this
      RawStoreBeginTransaction -> pure ()
      RawStoreRollback -> do
        (_, treeMap) <- liftIO $ readIORef treesVar
        version <- liftIO $ readIORef versionVar
        case version of
          Latest -> do
            let mOldTree = case Map.toDescList  treeMap of
                             []               -> Nothing
                             (_, oldTree) : _ -> Just oldTree
            liftIO $ writeIORef treesVar (fromMaybe AT.empty mOldTree, treeMap)
          _ -> throwSDKError $ RawStoreInvalidOperation "Rollback"
      RawStoreCommit -> do
        (headTree, treeMap) <- liftIO $ readIORef treesVar
        version <- liftIO $ readIORef versionVar
        case version of
          Latest ->
            let newVerion = case Map.toDescList treeMap of
                              []         -> 1
                              (k, _) : _ -> k + 1
                updatedTreeMap = Map.insert newVerion headTree treeMap
            in liftIO $ writeIORef treesVar (headTree, updatedTreeMap)
          _ -> throwSDKError $ RawStoreInvalidOperation "Commit"
    ) m

evalMergeScopes
  :: Members [Reader AuthTreeState,  Embed IO] r
  => Sem (MergeScopes ': r) a
  -> Sem r a
evalMergeScopes =
  interpret
    (\case
      MergeScopes -> do
        AuthTreeState{query, mempool, consensus} <- ask
        let AuthTree {versionVar=qVersionVar} = query
            AuthTree {versionVar=memVersionVar} = mempool
            AuthTree {treesVar} = consensus
        (_, treeMap) <- liftIO $ readIORef treesVar
        let latestVersion = case Map.toDescList treeMap of
                              []         -> Genesis
                              (k, _) : _ -> Version k
        forM_ [qVersionVar, memVersionVar] $ \ior -> liftIO $ writeIORef ior latestVersion
    )
