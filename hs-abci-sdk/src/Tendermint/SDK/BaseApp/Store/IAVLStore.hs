module Tendermint.SDK.BaseApp.Store.IAVLStore
  ( IAVLVersion
  , initGrpcClient
  --, initScopeVersions
  --, evalMergeScopes
  , evalTransaction
  , evalRead
  , evalWrite
  ) where


import           Control.Lens                          ((&), (.~), (^.))
import           Control.Monad                         (void)
import           Control.Monad.IO.Class
import           Data.IORef                            (IORef, readIORef)
import           Data.ProtoLens.Message                (defMessage)
import           Data.Text                             (pack)
import qualified Database.IAVL.RPC                     as IAVL
import           Database.IAVL.RPC.Types               (initGrpcClient)
import           Network.GRPC.Client                   (RawReply)
import           Network.GRPC.Client.Helpers           (GrpcClient)
import           Network.HTTP2.Client                  (ClientIO,
                                                        TooMuchConcurrency,
                                                        runClientIO)
import           Polysemy                              (Embed, Member, Members,
                                                        Sem, interpret)
import           Polysemy.Error                        (Error)
import qualified Proto.Iavl.Api_Fields                 as Api
import           Tendermint.SDK.BaseApp.Errors         (AppError, SDKError (..))
import           Tendermint.SDK.BaseApp.Store.RawStore (ReadStore (..),
                                                        Transaction (..),
                                                        WriteStore (..),
                                                        makeRawKey)
import           Tendermint.SDK.BaseApp.Store.Scope    (Version (..))

data IAVLVersion = IAVLVersion
  { iavlVersion :: IORef Version
  }

--initIAVLVersion :: Version -> IO IAVLVersion
--initIAVLVersion v = IAVLVersion <$> newIORef v

evalWrite
  :: Member (Embed IO) r
  => GrpcClient
  -> forall a. Sem (WriteStore ': r) a -> Sem r a
evalWrite gc m =
  interpret
    (\case
      StorePut k v ->
        let setReq = defMessage & Api.key .~ makeRawKey k
                                & Api.value .~ v
        in void . liftIO . runGrpc $ IAVL.set gc setReq
      StoreDelete k ->
        let remReq = defMessage & Api.key .~ makeRawKey k
        in void . liftIO . runGrpc $ IAVL.remove gc remReq
    ) m

evalRead
  :: Member (Embed IO) r
  => GrpcClient
  -> IAVLVersion
  -> forall a. Sem (ReadStore ': r) a -> Sem r a
evalRead gc IAVLVersion{iavlVersion} m = do
  version <- liftIO $ readIORef iavlVersion
  interpret
    (\case
      StoreGet k -> do
        case version of
          Latest -> do
            let getReq = defMessage & Api.key .~ makeRawKey k
            res <- liftIO . runGrpc $ IAVL.get gc getReq
            case res ^. Api.value of
              ""  -> pure Nothing
              val -> pure $ Just val
          Version v -> do
            let getVerReq = defMessage & Api.key .~ makeRawKey k
                                       & Api.version .~ fromInteger (toInteger v)
            res <- liftIO . runGrpc $ IAVL.getVersioned gc getVerReq
            case res ^. Api.value of
              ""  -> pure Nothing
              val -> pure $ Just val
          Genesis -> pure Nothing
      StoreProve _ -> pure Nothing
      StoreRoot _ -> do
        case version of
          Latest -> do
            res <- liftIO . runGrpc $ IAVL.hash gc
            pure $ res ^. Api.rootHash
          Genesis -> pure ""
          Version _ -> error "Root not yet implemented for numbered version"
    ) m

evalTransaction
  :: Members [Embed IO, Error AppError] r
  => GrpcClient
  -> forall a. Sem (Transaction ': r) a -> Sem r a
evalTransaction gc m = do
  interpret
    (\case
      -- NOTICE :: Currently unnecessary with the DB commit/version implementation.
      BeginTransaction -> pure ()
      Rollback -> void . liftIO . runGrpc $ IAVL.rollback gc
      Commit -> void . liftIO . runGrpc $ IAVL.saveVersion gc
    ) m

--evalMergeScopes
--  :: Members [Reader ScopeVersions,  Embed IO, Error AppError] r
--  => GrpcClient
--  -> Sem (MergeScopes ': r) a
--  -> Sem r a
--evalMergeScopes gc =
--  interpret
--    (\case
--      MergeScopes -> do
--        ScopeVersions{query, mempool} <- ask
--        let IAVLVersion queryV = query
--            IAVLVersion mempoolV = mempool
--        res <- runGrpc' $ IAVL.version gc
--        let version = Version . fromInteger . toInteger $ res ^. Api.version
--        forM_ [queryV, mempoolV] $ \ior -> liftIO $ writeIORef ior version
--    )

runGrpc
  :: ClientIO (Either TooMuchConcurrency (RawReply a))
  -> IO a
runGrpc f = runClientIO f >>= \case
  Right (Right (Right (_, _, Right res))) -> pure $  res
  Right (Right (Right (_, _, Left err))) -> error . show $ GrpcError (pack $ show err)
  Right (Right (Left err)) -> error . show $ GrpcError (pack $ show err)
  Right (Left err) -> error . show $ GrpcError (pack $ show err)
  Left err -> error . show $ GrpcError (pack $ show err)
