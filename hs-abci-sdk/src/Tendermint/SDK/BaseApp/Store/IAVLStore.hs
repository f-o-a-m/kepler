module Tendermint.SDK.BaseApp.Store.IAVLStore
  ( ScopeVersions(..)
  , IAVLVersionGetter(..)
  , initScopeVersions
  , evalMergeScopes
  , evalTagged
  ) where


import           Control.Lens                          ((&), (.~), (^.))
import           Control.Monad                         (void, forM_)
import           Control.Monad.IO.Class
import           Data.IORef                            (IORef, newIORef,
                                                        readIORef, writeIORef)

import           Data.ProtoLens.Message                (defMessage)
import           Data.Proxy
import           Data.Text                             (pack)
import qualified Database.IAVL.RPC                     as IAVL
import           Network.GRPC.Client                   (RawReply)
import           Network.GRPC.Client.Helpers           (GrpcClient
                                                        )
import           Network.HTTP2.Client                  (ClientIO,
                                                        TooMuchConcurrency,
                                                        runClientIO)
import           Polysemy                              (Embed, Members, Sem,
                                                        interpret)
import           Polysemy.Error                        (Error)
import           Polysemy.Reader                       (Reader, ask, asks)
import           Polysemy.Tagged                       (Tagged (..))
import qualified Proto.Iavl.Api_Fields                 as Api
import           Tendermint.SDK.BaseApp.Errors         (AppError, SDKError (..),
                                                        throwSDKError)
import           Tendermint.SDK.BaseApp.Store.RawStore (RawStore (..),
                                                        makeRawKey)
import           Tendermint.SDK.BaseApp.Store.Scope    (ConnectionScope (..),
                                                        MergeScopes (..))

data IAVLVersion (c :: ConnectionScope) = IAVLVersion
  { iavlVersion :: IORef Version
  }

initIAVLVersion :: Version -> IO (IAVLVersion c)
initIAVLVersion v = IAVLVersion <$> newIORef v

data Version = Latest
             | Version Integer


data ScopeVersions = ScopeVersions
  { query     :: IAVLVersion 'Query
  , mempool   :: IAVLVersion 'Mempool
  , consensus :: IAVLVersion 'Consensus
  }

initScopeVersions :: GrpcClient -> IO ScopeVersions
initScopeVersions gc = do
  eversion <- runGrpc $ IAVL.version gc
  case eversion of
    Left _ -> error "unable to get initial IAVL Version"
    Right res -> do
      let version = toInteger $ res ^. Api.version
      ScopeVersions <$> initIAVLVersion (Version version) <*> initIAVLVersion (Version version) <*> initIAVLVersion Latest

class IAVLVersionGetter (s :: ConnectionScope) where
  getIAVLVersion :: Proxy s -> ScopeVersions -> IAVLVersion s

instance IAVLVersionGetter 'Query where
  getIAVLVersion _ = query

instance IAVLVersionGetter 'Mempool where
  getIAVLVersion _ = mempool

instance IAVLVersionGetter 'Consensus where
  getIAVLVersion _ = consensus

evalTagged
  :: forall (s :: ConnectionScope) r.
     Members [Reader ScopeVersions, Error AppError, Embed IO] r
  => IAVLVersionGetter s
  => GrpcClient
  -> forall a. Sem (Tagged s RawStore ': r) a -> Sem r a
evalTagged gc m = do
  IAVLVersion{iavlVersion} <- asks (getIAVLVersion (Proxy :: Proxy s))
  interpret
    (\(Tagged action) -> case action of
      RawStorePut k v -> do
        version <- liftIO $ readIORef iavlVersion
        case version of
          Latest ->
            let setReq = defMessage & Api.key .~ makeRawKey k
                                    & Api.value .~ v
            in void . runGrpc' $ IAVL.set gc setReq
          Version _ -> throwSDKError $ RawStoreInvalidOperation "Put"
      RawStoreGet k -> do
        version <- liftIO $ readIORef iavlVersion
        case version of
          Latest -> do
            let getReq = defMessage & Api.key .~ makeRawKey k
            res <- runGrpc' $ IAVL.get gc getReq
            -- TODO :: Returns empty ByteString if not there, expected behavior?
            pure $ Just $ res ^. Api.value
          Version v -> do
            let getVerReq = defMessage & Api.key .~ makeRawKey k
                                       & Api.version .~ fromInteger v
            res <- runGrpc' $ IAVL.getVersioned gc getVerReq
            -- TODO :: Returns empty ByteString if not there, expected behavior?
            pure $ Just $ res ^. Api.value
      RawStoreProve _ -> pure Nothing
      RawStoreDelete k -> do
        version <- liftIO $ readIORef iavlVersion
        case version of
          Latest ->
            let remReq = defMessage & Api.key .~ makeRawKey k
            in void . runGrpc' $ IAVL.remove gc remReq
          Version _ -> throwSDKError $ RawStoreInvalidOperation "Delete"
      RawStoreRoot -> do
        version <- liftIO $ readIORef iavlVersion
        case version of
          Latest -> do
            res <- runGrpc' $ IAVL.hash gc
            pure $ res ^. Api.rootHash
          Version _ -> throwSDKError $ RawStoreInvalidOperation "Root"
      RawStoreBeginTransaction -> undefined
      RawStoreRollback -> do
        version <- liftIO $ readIORef iavlVersion
        case version of
          Latest -> do
            void . runGrpc' $ IAVL.rollback gc
          Version _ -> throwSDKError $ RawStoreInvalidOperation "Rollback"
      RawStoreCommit -> do
        version <- liftIO $ readIORef iavlVersion
        case version of
          Latest -> do
            void . runGrpc' $ IAVL.saveVersion gc
          Version _ -> throwSDKError $ RawStoreInvalidOperation "Commit"
    ) m

evalMergeScopes
  :: Members [Reader ScopeVersions,  Embed IO, Error AppError] r
  => GrpcClient
  -> Sem (MergeScopes ': r) a
  -> Sem r a
evalMergeScopes gc =
  interpret
    (\case
      MergeScopes -> do
        ScopeVersions{query, mempool} <- ask
        let IAVLVersion queryV = query
            IAVLVersion mempoolV = mempool
        res <- runGrpc' $ IAVL.version gc
        let version = Version . toInteger $ res ^. Api.version
        forM_ [queryV, mempoolV] $ \ior -> liftIO $ writeIORef ior version
    )

runGrpc'
  :: forall a r.
     Members [Error AppError, Embed IO] r
  => ClientIO (Either TooMuchConcurrency (RawReply a))
  -> Sem r a
runGrpc' f = liftIO (runGrpc f) >>= \case
  Left err -> throwSDKError err
  Right v -> pure v

runGrpc :: ClientIO (Either TooMuchConcurrency (RawReply a)) -> IO (Either SDKError a)
runGrpc f = runClientIO f >>= \case
  Right (Right (Right (_, _, Right res))) -> pure $ Right res
  Right (Right (Right (_, _, Left err))) -> pure $ Left $ GrpcError (pack $ show err)
  Right (Right (Left err)) -> pure $ Left $ GrpcError (pack $ show err)
  Right (Left err) -> pure $ Left $ GrpcError (pack $ show err)
  Left err -> pure $ Left $ GrpcError (pack $ show err)
