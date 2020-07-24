module Tendermint.SDK.BaseApp.Store.MemoryStore
  (
  -- * Environment
    DBVersions(..)
  , initDBVersions
  , DB
  , initDB
  -- * Eval
  , evalStoreEffs
  , evalRead
  , evalWrite
  ) where


import           Control.Monad.IO.Class                (liftIO)
import qualified Crypto.Data.Auth.Tree                 as AT
import qualified Crypto.Data.Auth.Tree.Class           as AT
import qualified Crypto.Data.Auth.Tree.Cryptonite      as Cryptonite
import qualified Crypto.Hash                           as Cryptonite
import           Data.ByteArray                        (convert)
import qualified Data.ByteArray.Base64String           as Base64
import           Data.ByteString                       (ByteString)
import           Data.IORef
import           Data.List                             (sortOn)
import           Data.Ord                              (Down (..))
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
                                                        makeKeyBytes)
import           Tendermint.SDK.Types.Effects          ((:&))



newtype AuthTreeHash =  AuthTreeHash (Cryptonite.Digest Cryptonite.SHA256)

instance AT.MerkleHash AuthTreeHash where
    emptyHash = AuthTreeHash Cryptonite.emptyHash
    hashLeaf k v = AuthTreeHash $ Cryptonite.hashLeaf k v
    concatHashes (AuthTreeHash a) (AuthTreeHash b) = AuthTreeHash $ Cryptonite.concatHashes a b

data DB = DB
  { dbCommitted :: IORef (AT.Tree Natural (AT.Tree ByteString ByteString))
  , dbLatest    :: IORef (AT.Tree ByteString ByteString)
  }

initDB :: IO DB
initDB =
  DB <$> newIORef AT.empty
     <*> newIORef AT.empty

evalWrite
  :: Member (Embed IO) r
  => DB
  -> forall a. Sem (WriteStore ': r) a -> Sem r a
evalWrite DB{dbLatest} m =
  interpret
    (\case
      StorePut k v ->
        liftIO . modifyIORef dbLatest $ AT.insert (makeKeyBytes k) v
      StoreDelete k ->
        liftIO . modifyIORef dbLatest $ AT.delete (makeKeyBytes k)
    ) m

evalRead
  :: Member (Embed IO) r
  => DB
  -> IORef Version
  -> forall a. Sem (ReadStore ': r) a -> Sem r a
evalRead DB{dbCommitted,dbLatest} iavlVersion m = do
  interpret
    (\case
      StoreGet k -> do
        version <- liftIO $ readIORef iavlVersion
        case version of
          Latest -> do
            tree <- liftIO $ readIORef dbLatest
            pure $ AT.lookup (makeKeyBytes k) tree
          Version v -> do
            tree <- liftIO $ readIORef dbCommitted
            pure (AT.lookup v tree >>= AT.lookup (makeKeyBytes k))
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
      Rollback -> liftIO $ do
          c <- getRecentCommit db
          writeIORef dbLatest c
      Commit -> liftIO $ do
        l <- readIORef dbLatest
        v <- makeCommit db l
        root <- getRootHash db
        pure $ CommitResponse
          { rootHash = Base64.fromBytes root
          , newVersion = fromInteger . toInteger $ v
          }
    ) m

evalCommitBlock
  :: Member (Embed IO) r
  => DB
  -> DBVersions
  -> forall a. Sem (CommitBlock ': r) a -> Sem r a
evalCommitBlock db DBVersions{..} = do
  interpret
    (\case
      CommitBlock -> liftIO $ do
        mv <- getVersion db
        writeIORef committed $ maybe Genesis Version
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
  vs@DBVersions{..} <- ask
  db <- ask
  evalCommitBlock db vs .
    evalTransaction db .
    evalWrite db .
    untag .
    evalRead db committed .
    untag .
    evalRead db latest .
    untag $ action

getRootHash
  :: DB
  -> IO ByteString
getRootHash db@DB{dbCommitted} = do
  mcv <- getVersion db
  case mcv of
    Nothing -> pure ""
    Just v -> do
      c <- readIORef dbCommitted
      case AT.lookup v c of
        Nothing -> pure ""
        Just tree ->
          let AuthTreeHash hash = AT.merkleHash tree
          in pure $ convert hash

getVersion
  :: DB
  -> IO (Maybe Natural)
getVersion DB{..}= do
  c <- readIORef dbCommitted
  pure $
    if c == AT.empty
      then Nothing
      else Just $ maximum $ map fst $ AT.toList c

getRecentCommit
  :: DB
  -> IO (AT.Tree ByteString ByteString)
getRecentCommit DB{..} = do
  c <- readIORef dbCommitted
  case sortOn (Down . fst) $ AT.toList c of
    []    -> pure AT.empty
    a : _ -> pure $ snd a

makeCommit
  :: DB
  -> AT.Tree ByteString ByteString
  -> IO Natural
makeCommit db@DB{dbCommitted} commit = do
  mv <- getVersion db
  let v = maybe 0 (+1) mv
  modifyIORef dbCommitted $ AT.insert v commit
  pure v
