module Tendermint.SDK.Test.IAVLStoreSpec (spec) where

import           Control.Lens                           (iso)
import           Control.Monad                          (void)
import           Control.Monad.IO.Class                 (liftIO)
import           Data.Bifunctor                         (first)
import           Data.ByteString                        (ByteString)
import qualified Data.Serialize                         as Serialize
import           Data.String.Conversions                (cs)
import           Network.GRPC.Client.Helpers            (GrpcClient)
import           Polysemy                               (Embed, Sem, runM)
import           Polysemy.Error                         (Error, runError)
import           Polysemy.Reader                        (Reader, runReader)
import           Polysemy.Resource                      (Resource, resourceToIO)
import           Polysemy.Tagged                        (tag)
import           Tendermint.SDK.BaseApp.Errors          (AppError (..), SDKError (InternalError),
                                                         throwSDKError)
import           Tendermint.SDK.BaseApp.Store           (IsKey (..),
                                                         RawKey (..),
                                                         Scope (..), StoreEffs,
                                                         StoreKey (..),
                                                         WriteStore, commit,
                                                         commitBlock, delete,
                                                         get, put, withSandbox,
                                                         withTransaction)
import           Tendermint.SDK.BaseApp.Store.IAVLStore (IAVLVersions,
                                                         evalStoreEffs,
                                                         initGrpcClient,
                                                         initIAVLVersions)
import           Tendermint.SDK.Codec                   (HasCodec (..))
import           Tendermint.SDK.Types.Effects           ((:&))
import           Test.Hspec

spec :: Spec
spec = beforeAll beforeAction $
  describe "IAVLStore" $ do

    it "can fail to query an empty AuthTreeStore" $ \driver -> do
      Right mv <- runIAVL driver $ tag @'QueryAndMempool $
        get storeKey IntStoreKey
      mv `shouldBe` Nothing

    it "can set a value and query the value" $ \driver -> do
      Right mv <- runIAVL driver $ do
        tag @'Consensus $ (put storeKey IntStoreKey (IntStore 1) :: Sem (WriteStore ': Effs) ())
        tag @'Consensus $ get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes and roll back" $ \driver -> do
      Right mv'' <- runIAVL driver $ do
        void $ withTransaction $
          tag @'Consensus $ (put storeKey IntStoreKey (IntStore 1)  :: Sem (WriteStore ': Effs) ())
        withSandbox $ do
          tag @'Consensus $ (put storeKey IntStoreKey (IntStore 5)  :: Sem (WriteStore ': Effs) ())
          mv <- tag @'Consensus $ get storeKey IntStoreKey
          liftIO (mv `shouldBe` Just (IntStore 5))

          tag @'Consensus $ (delete storeKey IntStoreKey  :: Sem (WriteStore ': Effs) ())
          mv' <- tag @'Consensus $ get storeKey IntStoreKey

          liftIO (mv' `shouldBe` Nothing)
        tag @'Consensus $ get storeKey IntStoreKey
      mv'' `shouldBe` Just (IntStore 1)

    it "can roll back if an error occurs during a transaction" $ \driver -> do
      Left apperr <- runIAVL driver $ do
        void $ withTransaction $
          tag @'Consensus $ (put storeKey IntStoreKey (IntStore 1)  :: Sem (WriteStore ': Effs) ())
        void $ withTransaction $ do
          tag @'Consensus $ (put storeKey IntStoreKey (IntStore 6)  :: Sem (WriteStore ': Effs) ())
          throwSDKError InternalError
      appErrorCode apperr `shouldBe` 1
      Right mv <- runIAVL driver $
        tag @'Consensus $ get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes with a transaction" $ \driver -> do
      Right (mv, _) <- runIAVL driver . withTransaction $ do
        tag @'Consensus $ (put storeKey IntStoreKey (IntStore 5)  :: Sem (WriteStore ': Effs) ())
        tag @'Consensus $ get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 5)

    it "can merge the scopes" $ \driver -> do
      -- set all to be initially the same value
      void $ runIAVL driver $
        withTransaction $
          tag @'Consensus $ (put storeKey IntStoreKey (IntStore 0)  :: Sem (WriteStore ': Effs) ())

      -- mergeScopes so that all are using the latest version
      void $ runIAVL driver $ commitBlock

      void $ runIAVL driver $ do
        res <- tag @'QueryAndMempool $ get storeKey IntStoreKey
        liftIO (res `shouldBe` (Just 0))
      void $ runIAVL driver $ do
        res <- tag @'Consensus $ get storeKey IntStoreKey
        liftIO (res `shouldBe` (Just 0))


      -- Make another change on Consensus that does not commit
      void $ runIAVL driver $ do
        tag @'Consensus $ (put storeKey IntStoreKey (IntStore 1)  :: Sem (WriteStore ': Effs) ())

      void $ runIAVL driver $ do
        res <- tag @'QueryAndMempool $ get storeKey IntStoreKey
        liftIO (res `shouldBe` (Just 0))

      void $ runIAVL driver $ do
        res <- tag @'Consensus $ get storeKey IntStoreKey
        liftIO (res `shouldBe` (Just 1))

      -- commit the changes
      void $ runIAVL driver $ commit

      -- mergeScopes so that all are using the latest version
      void $ runIAVL driver $ commitBlock

      void $ runIAVL driver $ do
        res <- tag @'QueryAndMempool $ get storeKey IntStoreKey
        liftIO (res `shouldBe` (Just 1))

      void $ runIAVL driver $ do
        res <- tag @'Consensus $ get storeKey IntStoreKey
        liftIO (res `shouldBe` (Just 1))


beforeAction :: IO (IAVLVersions, GrpcClient)
beforeAction = do
  vs <- initIAVLVersions
  gc  <- initGrpcClient
  pure (vs, gc)

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

storeKey :: StoreKey "int_store"
storeKey = StoreKey "int_store"

type Effs =
  StoreEffs :& [Reader IAVLVersions, Reader GrpcClient, Error AppError, Resource, Embed IO]

runIAVL
  :: (IAVLVersions, GrpcClient)
  -> forall a.
     Sem Effs a
  -> IO (Either AppError a)
runIAVL (versions, gc) =
  runM .
    resourceToIO .
    runError .
    runReader gc .
    runReader versions .
    evalStoreEffs
