module Tendermint.SDK.Test.IAVLStoreSpec (spec) where

import           Control.Lens                             (iso)
import           Control.Monad                            (void)
import           Control.Monad.IO.Class                   (liftIO)
import           Data.Bifunctor                           (first)
import           Data.ByteString                          (ByteString)
import           Data.Proxy
import qualified Data.Serialize                           as Serialize
import           Data.String.Conversions                  (cs)
import           Network.GRPC.Client.Helpers              (GrpcClient)
import           Polysemy                                 (Embed, Members, Sem,
                                                           runM)
import           Polysemy.Error                           (Error, runError)
import           Polysemy.Reader                          (Reader, runReader)
import           Polysemy.Resource                        (Resource,
                                                           resourceToIO)
import           Polysemy.Tagged                          (tag)
import           Tendermint.SDK.BaseApp.Errors            (AppError (..), SDKError (InternalError),
                                                           throwSDKError)
import           Tendermint.SDK.BaseApp.Store             (IsKey (..),
                                                           KeyRoot (..),
                                                           RawKey (..),
                                                           ReadStore,
                                                           Scope (..), Store,
                                                           StoreEffs,
                                                           WriteStore,
                                                           WriteStore, commit,
                                                           commitBlock, delete,
                                                           get, makeStore, put,
                                                           withSandbox,
                                                           withTransaction)
import           Tendermint.SDK.BaseApp.Store.IAVLStore   (GrpcConfig (..),
                                                           IAVLVersions,
                                                           evalStoreEffs,
                                                           initGrpcClient,
                                                           initIAVLVersions)
import qualified Tendermint.SDK.BaseApp.Store.MemoryStore as Memory
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Tendermint.SDK.Types.Effects             ((:&))
import           Test.Hspec

spec :: Spec
spec = do
  beforeAll iavlBeforeAction $
    describe "IAVL tests" $ spec' (Proxy @IAVLEffs)

  beforeAll pureBeforeAction $
    describe "Pure tests" $ spec' (Proxy @PureEffs)

spec' :: Members [Error AppError, Embed IO, Resource] r => Members StoreEffs r => Proxy r -> SpecWith (Driver r)
spec' (Proxy :: Proxy r) = do

    it "can fail to query an empty AuthTreeStore" $ \(driver :: Driver r) -> do
      Right mv <- runDriver driver $ tag @'QueryAndMempool $
        get store IntStoreKey
      mv `shouldBe` Nothing

    it "can set a value and query the value" $ \(driver :: Driver r) -> do
      Right mv <- runDriver driver $ do
        tag @'Consensus @WriteStore $ (put store IntStoreKey (IntStore 1) :: Sem (WriteStore ': r) ())
        tag @'Consensus @ReadStore $ get store IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes and roll back" $ \(driver :: Driver r) -> do
      Right mv'' <- runDriver driver $ do
        void $ withTransaction $
          tag @'Consensus $ (put store IntStoreKey (IntStore 1)  :: Sem (WriteStore ': r) ())
        withSandbox $ do
          tag @'Consensus @WriteStore $ (put store IntStoreKey (IntStore 5)  :: Sem (WriteStore ': r) ())
          mv <- tag @'Consensus @ReadStore $ get store IntStoreKey
          liftIO (mv `shouldBe` Just (IntStore 5))

          tag @'Consensus @WriteStore$ (delete store IntStoreKey  :: Sem (WriteStore ': r) ())
          mv' <- tag @'Consensus @ReadStore $ get store IntStoreKey

          liftIO (mv' `shouldBe` Nothing)
        tag @'Consensus @ReadStore $ get store IntStoreKey
      mv'' `shouldBe` Just (IntStore 1)

    it "can roll back if an error occurs during a transaction" $ \driver -> do
      Left apperr <- runDriver driver $ do
        void $ withTransaction $
          tag @'Consensus $ (put store IntStoreKey (IntStore 1)  :: Sem (WriteStore ': r) ())
        void $ withTransaction $ do
          tag @'Consensus $ (put store IntStoreKey (IntStore 6)  :: Sem (WriteStore ': r) ())
          throwSDKError $ InternalError "SomeError"
      appErrorCode apperr `shouldBe` 1
      Right mv <- runDriver driver $
        tag @'Consensus @ReadStore $ get store IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes with a transaction" $ \driver -> do
      Right (mv, _) <- runDriver driver . withTransaction $ do
        tag @'Consensus $ (put store IntStoreKey (IntStore 5)  :: Sem (WriteStore ': r) ())
        tag @'Consensus @ReadStore $ get store IntStoreKey
      mv `shouldBe` Just (IntStore 5)

    it "can merge the scopes" $ \driver -> do
      -- set all to be initially the same value
      void $ runDriver driver $
        withTransaction $
          tag @'Consensus $ (put store IntStoreKey (IntStore 0)  :: Sem (WriteStore ': r) ())

      -- mergeScopes so that all are using the latest version
      void $ runDriver driver $ commitBlock

      void $ runDriver driver $ do
        res <- tag @'QueryAndMempool $ get store IntStoreKey
        liftIO (res `shouldBe` Just 0)
      void $ runDriver driver $ do
        res <- tag @'Consensus @ReadStore $ get store IntStoreKey
        liftIO (res `shouldBe` Just 0)


      -- Make another change on Consensus that does not commit
      void $ runDriver driver $ do
        tag @'Consensus $ (put store IntStoreKey (IntStore 1)  :: Sem (WriteStore ': r) ())

      void $ runDriver driver $ do
        res <- tag @'QueryAndMempool @ReadStore $ get store IntStoreKey
        liftIO (res `shouldBe` Just 0)

      void $ runDriver driver $ do
        res <- tag @'Consensus @ReadStore $ get store IntStoreKey
        liftIO (res `shouldBe` Just 1)

      -- commit the changes
      void $ runDriver driver $ commit

      -- mergeScopes so that all are using the latest version
      void $ runDriver driver $ commitBlock

      void $ runDriver driver $ do
        res <- tag @'QueryAndMempool @ReadStore $ get store IntStoreKey
        liftIO (res `shouldBe` Just 1)

      void $ runDriver driver $ do
        res <- tag @'Consensus @ReadStore $ get store IntStoreKey
        liftIO (res `shouldBe` Just 1)


iavlBeforeAction :: IO (Driver IAVLEffs)
iavlBeforeAction = do
  vs <- initIAVLVersions
  gc  <- initGrpcClient $ GrpcConfig "0.0.0.0" 8090
  pure $ runIAVL (vs, gc)

pureBeforeAction :: IO (Driver PureEffs)
pureBeforeAction = do
  vs <- Memory.initDBVersions
  db  <- Memory.initDB
  pure $ runPure (vs, db)

newtype IntStore = IntStore Int deriving (Eq, Show, Num, Serialize.Serialize)

data IntStoreKey = IntStoreKey

instance HasCodec IntStore where
    encode = Serialize.encode
    decode = first cs . Serialize.decode

instance RawKey IntStoreKey where
    rawKey = iso (\_ -> cs intStoreKey) (const IntStoreKey)
      where
        intStoreKey :: ByteString
        intStoreKey = "IntStore"

instance IsKey IntStoreKey "int_store" where
    type Value IntStoreKey "int_store" = IntStore

store :: Store "int_store"
store = makeStore $ KeyRoot "int_store"

type IAVLEffs =
  StoreEffs :& [Reader IAVLVersions, Reader GrpcClient, Error AppError, Resource, Embed IO]

runIAVL
  :: (IAVLVersions, GrpcClient)
  -> Driver IAVLEffs
runIAVL (versions, gc) = Driver $ \action ->
  runM .
      resourceToIO .
      runError .
      runReader gc .
      runReader versions $
      evalStoreEffs action


type PureEffs =
  StoreEffs :& [Reader Memory.DBVersions, Reader Memory.DB, Error AppError, Resource, Embed IO]

runPure
  :: (Memory.DBVersions, Memory.DB)
  -> Driver PureEffs
runPure (versions, db) = Driver $ \action ->
  runM .
      resourceToIO .
      runError .
      runReader db .
      runReader versions $
      Memory.evalStoreEffs action


newtype Driver core = Driver
  (forall a. Sem core a -> IO (Either AppError a))

runDriver
  :: forall core.
     Driver core
  -> forall a.
     Sem core a
  -> IO (Either AppError a)
runDriver (Driver f) a = f a
