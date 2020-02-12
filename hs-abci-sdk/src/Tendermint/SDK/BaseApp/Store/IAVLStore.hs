module Tendermint.SDK.BaseApp.Store.IAVLStore
  ( IAVLVersion(..)
  , IAVLVersions(..)
  , GrpcClient
  , GrpcConfig(..)
  , initGrpcClient
  , initIAVLVersions
  , evalTransaction
  , evalCommitBlock
  , evalRead
  , evalWrite
  , evalStoreEffs
  ) where


import           Control.Lens                          ((&), (.~), (^.))
import           Control.Monad                         (void)
import           Control.Monad.IO.Class
import           Data.ByteArray.Base64String           (fromBytes)
import           Data.IORef                            (IORef, newIORef,
                                                        readIORef, writeIORef)
import           Data.ProtoLens.Message                (defMessage)
import           Data.Text                             (pack)
import qualified Database.IAVL.RPC                     as IAVL
import           Database.IAVL.RPC.Types               (GrpcConfig (..),
                                                        initGrpcClient)
import           Network.GRPC.Client                   (RawReply)
import           Network.GRPC.Client.Helpers           (GrpcClient)
import           Network.HTTP2.Client                  (ClientIO,
                                                        TooMuchConcurrency,
                                                        runClientIO)
import           Numeric.Natural                       (Natural)
import           Polysemy                              (Embed, Member, Members,
                                                        Sem, interpret)
import           Polysemy.Error                        (Error)
import           Polysemy.Reader                       (Reader, ask)
import           Polysemy.Tagged                       (untag)
import qualified Proto.Iavl.Api_Fields                 as Api
import           Tendermint.SDK.BaseApp.Errors         (AppError, SDKError (..))
import           Tendermint.SDK.BaseApp.Store.RawStore (CommitBlock (..),
                                                        CommitResponse (..),
                                                        ReadStore (..),
                                                        StoreEffs,
                                                        Transaction (..),
                                                        WriteStore (..),
                                                        makeRawKey)
import           Tendermint.SDK.Types.Effects          ((:&))

data IAVLVersion =
    Genesis
  | Version Natural
  | Latest

data IAVLVersions = IAVLVersions
  { latest    :: IORef IAVLVersion
  , committed :: IORef IAVLVersion
  }

initIAVLVersions :: IO IAVLVersions
initIAVLVersions = IAVLVersions <$> newIORef Latest <*> newIORef Genesis

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
  -> IORef IAVLVersion
  -> forall a. Sem (ReadStore ': r) a -> Sem r a
evalRead gc iavlVersion m = do
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
      Commit -> do
        resp <- liftIO . runGrpc $ IAVL.saveVersion gc
        pure $ CommitResponse
          { rootHash = fromBytes (resp ^. Api.rootHash)
          , newVersion = fromInteger . toInteger $ resp ^. Api.version
          }
    ) m

evalCommitBlock
  :: Members [Embed IO, Error AppError] r
  => GrpcClient
  -> IAVLVersions
  -> forall a. Sem (CommitBlock ': r) a -> Sem r a
evalCommitBlock gc IAVLVersions{committed} = do
  interpret
    (\case
      CommitBlock -> do
        versionResp <- liftIO . runGrpc $ IAVL.version gc
        let version = Version . fromInteger . toInteger $ versionResp ^. Api.version
        liftIO $ writeIORef committed version
        hashResp <- liftIO . runGrpc $ IAVL.hash gc
        pure . fromBytes $ hashResp ^. Api.rootHash
    )

evalStoreEffs
  :: Members [Embed IO, Reader IAVLVersions, Error AppError, Reader GrpcClient] r
  => forall a.
     Sem (StoreEffs :& r) a
  -> Sem r a
evalStoreEffs action = do
  vs@IAVLVersions{..} <- ask
  grpc <- ask
  evalCommitBlock grpc vs .
    evalTransaction grpc .
    evalWrite grpc .
    untag .
    evalRead grpc committed .
    untag .
    evalRead grpc latest .
    untag $ action

runGrpc
  :: ClientIO (Either TooMuchConcurrency (RawReply a))
  -> IO a
runGrpc f = runClientIO f >>= \case
  Right (Right (Right (_, _, Right res))) -> pure $  res
  Right (Right (Right (_, _, Left err))) -> error . show $ GrpcError (pack $ show err)
  Right (Right (Left err)) -> error . show $ GrpcError (pack $ show err)
  Right (Left err) -> error . show $ GrpcError (pack $ show err)
  Left err -> error . show $ GrpcError (pack $ show err)
