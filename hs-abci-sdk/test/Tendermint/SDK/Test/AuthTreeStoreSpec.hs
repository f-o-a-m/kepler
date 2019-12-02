module Tendermint.SDK.Test.AuthTreeStoreSpec where

import           Control.Lens                 (iso)
import           Data.Bifunctor               (first)
import           Data.ByteString              (ByteString)
import qualified Data.Serialize               as Serialize
import           Data.String.Conversions      (cs)
import           Polysemy                     (Embed, Sem, runM)
import           Polysemy.Error               (Error, runError)
import           Polysemy.Resource            (Resource, resourceToIO)
import           Tendermint.SDK.AuthTreeStore (AuthTree, eval, initAuthTree)
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Errors        (AppError (..),
                                               SDKError (InternalError),
                                               throwSDKError)
import           Tendermint.SDK.Store         (IsKey (..), RawKey (..),
                                               RawStore, StoreKey (..), delete,
                                               get, put,
                                               rawStoreBeginTransaction,
                                               rawStoreRollback,
                                               withTransaction)
import           Test.Hspec

spec :: Spec
spec = beforeAll beforeAction $
  describe "AuthTreeStore" $ do
    it "can fail to query an empty AuthTreeStore" $ \driver -> do
      Right mv <- runAuthTree driver $ get storeKey IntStoreKey
      mv `shouldBe` Nothing
    it "can set a value and query the value" $ \driver -> do
      Right mv <- runAuthTree driver $ do
        put storeKey IntStoreKey (IntStore 1)
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)
    it "can make changes and roll back" $ \driver -> do
      Right mv <- runAuthTree driver $ do
        rawStoreBeginTransaction
        put storeKey IntStoreKey (IntStore 5)
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 5)

      Right mv' <- runAuthTree driver $ do
        delete storeKey IntStoreKey
        get storeKey IntStoreKey
      mv' `shouldBe` Nothing

      Right mv'' <- runAuthTree driver $ do
        rawStoreRollback
        get storeKey IntStoreKey
      mv'' `shouldBe` Just (IntStore 1)
    it "can roll back if an error occurs during a transaction" $ \driver -> do
      Left apperr <- runAuthTree driver . withTransaction $ do
        put storeKey IntStoreKey (IntStore 5)
        throwSDKError InternalError
      appErrorCode apperr `shouldBe` 1
      Right mv <- runAuthTree driver $ get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)
    it "can make changes with a transaction" $ \driver -> do
      Right mv <- runAuthTree driver . withTransaction $ do
        put storeKey IntStoreKey (IntStore 5)
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 5)


beforeAction :: IO AuthTree
beforeAction = initAuthTree

newtype IntStore = IntStore Int deriving (Eq, Show, Serialize.Serialize)

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
  :: AuthTree
  -> Sem [RawStore, Error AppError, Resource, Embed IO] a
  -> IO (Either AppError a)
runAuthTree driver = runM . resourceToIO . runError . eval driver
