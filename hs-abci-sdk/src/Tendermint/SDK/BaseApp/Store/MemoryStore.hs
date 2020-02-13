module Tendermint.SDK.BaseApp.Store.MemoryStore where


import           Control.Monad.IO.Class                (liftIO)
import qualified Crypto.Data.Auth.Tree                 as AT
import qualified Crypto.Data.Auth.Tree.Class           as AT
import qualified Crypto.Data.Auth.Tree.Cryptonite      as Cryptonite
import qualified Crypto.Hash                           as Cryptonite
import           Data.ByteArray                        (convert)
import qualified Data.ByteArray.Base64String           as Base64
import           Data.ByteString                       (ByteString)
import           Data.IORef
import           Data.Maybe                            (fromMaybe)
import           Debug.Trace                           as Trace
import           Numeric.Natural                       (Natural)
import           Polysemy
import           Polysemy.Reader                       (Reader, ask)
import           Polysemy.Tagged                       (untag)
import           Tendermint.SDK.BaseApp.Store.RawStore (CommitBlock (..),
                                                        CommitResponse (..),
                                                        ReadStore (..),
                                                        StoreEffs,
                                                        Transaction (..),
                                                        Version (..),
                                                        WriteStore (..),
                                                        makeRawKey)
import           Tendermint.SDK.Types.Effects          ((:&))



newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b

data DB = DB
  { dbCommitted    :: IORef (AT.Tree Natural (AT.Tree ByteString ByteString))
  , dbLatest       :: IORef (AT.Tree ByteString ByteString)
  , currentVersion :: IORef (Maybe Natural)
  }

getRootHash
  :: DB
  -> IO ByteString
getRootHash DB{currentVersion, dbCommitted} = do
  mcv <- readIORef currentVersion
  case mcv of
    Nothing -> pure ""
    Just v -> do
      cs <- readIORef dbCommitted
      case AT.lookup v cs of
        Nothing -> pure ""
        Just tree ->
          let AuthTreeHash hash = AT.merkleHash tree
          in pure $ convert hash

initDB :: IO DB
initDB =
  DB <$> newIORef AT.empty
     <*> newIORef AT.empty
     <*> newIORef Nothing

evalWrite
  :: Member (Embed IO) r
  => DB
  -> forall a. Sem (WriteStore ': r) a -> Sem r a
evalWrite DB{dbLatest} m =
  interpret
    (\case
      StorePut k v -> do
        liftIO . modifyIORef dbLatest $ AT.insert (makeRawKey k) v
      StoreDelete k ->
        liftIO . modifyIORef dbLatest $ AT.delete (makeRawKey k)
    ) m

evalRead
  :: Member (Embed IO) r
  => DB
  -> IORef Version
  -> forall a. Sem (ReadStore ': r) a -> Sem r a
evalRead DB{dbCommitted,dbLatest} iavlVersion m = do
  Trace.traceM "evalRead"
  interpret
    (\case
      StoreGet k -> do
        version <- liftIO $ readIORef iavlVersion
        Trace.traceM $ "Looking up version " <> show version
        case version of
          Latest -> do
            tree <- liftIO $ readIORef dbLatest
            pure $ AT.lookup (makeRawKey k) tree
          Version v -> do
            tree <- liftIO $ readIORef dbCommitted
            pure (AT.lookup v tree >>= AT.lookup (makeRawKey k))
          Genesis -> pure Nothing
      StoreProve _ -> pure Nothing
    ) m

evalTransaction
  :: Member (Embed IO) r
  => DB
  -> forall a. Sem (Transaction ': r) a -> Sem r a
evalTransaction db@DB{..} m = do
  interpret
    (\case
      -- NOTICE :: Currently unnecessary with the DB commit/version implementation.
      BeginTransaction -> pure ()
      Rollback -> liftIO $ modifyIORef dbLatest (const AT.empty)
      Commit -> liftIO $ do
        Trace.traceM "Commit interpreter"
        l <- readIORef dbLatest
        mcv <- readIORef currentVersion
        let cv = fromMaybe 0 mcv
        modifyIORef dbCommitted $ AT.insert (cv + 1) l
        root <- getRootHash db
        pure $ CommitResponse
          { rootHash = Base64.fromBytes root
          , newVersion = fromInteger . toInteger $ cv
          }
    ) m

evalCommitBlock
  :: Member (Embed IO) r
  => DB
  -> forall a. Sem (CommitBlock ': r) a -> Sem r a
evalCommitBlock db@DB{..} = do
  interpret
    (\case
      CommitBlock -> liftIO $ do
        version <- readIORef currentVersion
        Trace.traceM $ "QueryMempool is now version " <> show version
        writeIORef currentVersion $ case version of
            Nothing -> Just 0
            Just v  -> Just $ v + 1
        root <- getRootHash db
        pure . Base64.fromBytes $ root
    )

data DBVersions = DBVersions
  { latest    :: IORef Version
  , committed :: IORef Version
  }

initDBVersions :: IO DBVersions
initDBVersions = DBVersions <$> newIORef Latest <*> newIORef Genesis

evalStoreEffs
  :: Members [Embed IO, Reader DBVersions, Reader DB] r
  => forall a.
     Sem (StoreEffs :& r) a
  -> Sem r a
evalStoreEffs action = do
  DBVersions{..} <- ask
  db <- ask
  evalCommitBlock db .
    evalTransaction db .
    evalWrite db .
    untag .
    evalRead db committed .
    untag .
    evalRead db latest .
    untag $ action
