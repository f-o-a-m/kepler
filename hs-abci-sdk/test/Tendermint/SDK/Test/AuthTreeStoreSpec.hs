module Tendermint.SDK.Test.AuthTreeStoreSpec (spec) where

import           Control.Lens                               (iso)
import           Control.Monad                              (void)
import           Control.Monad.IO.Class                     (liftIO)
import           Data.Bifunctor                             (first)
import           Data.ByteString                            (ByteString)
import qualified Data.Serialize                             as Serialize
import           Data.String.Conversions                    (cs)
import           Polysemy                                   (Embed, Sem, runM)
import           Polysemy.Error                             (Error, runError)
import           Polysemy.Reader                            (Reader, runReader)
import           Polysemy.Resource                          (Resource,
                                                             resourceToIO)
import           Polysemy.Tagged                            (Tagged)
import           Tendermint.SDK.BaseApp.Errors              (AppError (..), SDKError (InternalError),
                                                             throwSDKError)
import           Tendermint.SDK.BaseApp.Store               (ConnectionScope (..),
                                                             IsKey (..),
                                                             MergeScopes,
                                                             RawKey (..),
                                                             RawStore,
                                                             StoreKey (..),
                                                             applyScope,
                                                             commitBlock,
                                                             delete, get,
                                                             mergeScopes, put,
                                                             withSandbox,
                                                             withTransaction)
import           Tendermint.SDK.BaseApp.Store.AuthTreeStore (AuthTreeGetter (..),
                                                             AuthTreeState,
                                                             evalMergeScopes,
                                                             evalTagged,
                                                             initAuthTreeState)
import           Tendermint.SDK.Codec                       (HasCodec (..))
import           Test.Hspec

spec :: Spec
spec = beforeAll beforeAction $
  describe "AuthTreeStore" $ do

    it "can fail to query an empty AuthTreeStore" $ \driver -> do
      Right mv <- runAuthTree driver $ applyScope @'Query $
        get storeKey IntStoreKey
      mv `shouldBe` Nothing

    it "can set a value and query the value" $ \driver -> do
      Right mv <- runAuthTree driver $ applyScope @'Consensus $ do
        put storeKey IntStoreKey (IntStore 1)
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes and roll back" $ \driver -> do
      Right mv'' <- runAuthTree driver $ applyScope @'Consensus $ do
        withTransaction $ put storeKey IntStoreKey (IntStore 1)
        withSandbox $ do
          put storeKey IntStoreKey (IntStore 5)
          mv <- get storeKey IntStoreKey
          liftIO (mv `shouldBe` Just (IntStore 5))

          delete storeKey IntStoreKey
          mv' <- get storeKey IntStoreKey

          liftIO (mv' `shouldBe` Nothing)
        get storeKey IntStoreKey
      mv'' `shouldBe` Just (IntStore 1)

    it "can roll back if an error occurs during a transaction" $ \driver -> do
      Left apperr <- runAuthTree driver $ applyScope @'Consensus $ do
        withTransaction $ put storeKey IntStoreKey (IntStore 1)
        withTransaction $ do
          put storeKey IntStoreKey (IntStore 6)
          throwSDKError InternalError
      appErrorCode apperr `shouldBe` 1
      Right mv <- runAuthTree driver $ applyScope @'Consensus $
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes with a transaction" $ \driver -> do
      Right mv <- runAuthTree driver . applyScope @'Consensus . withTransaction $ do
        put storeKey IntStoreKey (IntStore 5)
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 5)

    it "can merge the scopes" $ \driver -> do
      -- commit initial value
      void $ runAuthTree driver . applyScope @'Consensus $
        withTransaction $ put storeKey IntStoreKey (IntStore 0)

      -- mergeScopes so that all are using the latest version
      void $ runAuthTree driver . applyScope @'Consensus $ mergeScopes

      -- check value changes for all scopes
      void $ runAuthTree driver . applyScope @'Query $
        get storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
      void $ runAuthTree driver . applyScope @'Mempool $
        get  storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
      void $ runAuthTree driver . applyScope @'Consensus $
        get storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)


      -- Make another change on Consensus that does not commit
      void $ runAuthTree driver . applyScope @'Consensus $
        put storeKey IntStoreKey (IntStore 1)

      -- check value changes for for only consensus
      void $ runAuthTree driver . applyScope @'Query $
        get storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
      void $ runAuthTree driver . applyScope @'Mempool $
        get  storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
      void $ runAuthTree driver . applyScope @'Consensus $
        get  storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)

      -- commit the changes
      void $ runAuthTree driver . applyScope @'Consensus $ commitBlock

      -- mergeScopes so that all are using the latest version
      void $ runAuthTree driver . applyScope @'Consensus $ mergeScopes

      -- check that the changes apply to scopes
      void $ runAuthTree driver . applyScope @'Query $
        get storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)
      void $ runAuthTree driver . applyScope @'Mempool $
        get  storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)
      void $ runAuthTree driver . applyScope @'Consensus $
        get  storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)




beforeAction :: IO AuthTreeState
beforeAction = initAuthTreeState

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

runAuthTree
  :: AuthTreeGetter s
  => AuthTreeState
  -> Sem [ Tagged s RawStore
         , MergeScopes
         , Reader AuthTreeState
         , Error AppError
         , Resource
         , Embed IO
         ] a
  -> IO (Either AppError a)
runAuthTree driver =
  runM .
  resourceToIO .
  runError .
  runReader driver .
  evalMergeScopes .
  evalTagged
