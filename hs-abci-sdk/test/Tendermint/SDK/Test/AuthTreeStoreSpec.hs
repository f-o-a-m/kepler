module Tendermint.SDK.Test.AuthTreeStoreSpec where

import           Control.Lens                 (iso)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Bifunctor               (first)
import           Data.ByteString              (ByteString)
import qualified Data.Serialize               as Serialize
import           Data.String.Conversions      (cs)
import           Polysemy                     (Embed, Sem, runM)
import           Polysemy.Error               (Error, runError)
import           Polysemy.Reader              (Reader, runReader)
import           Polysemy.Resource            (Resource, resourceToIO)
import           Polysemy.Tagged
import           Tendermint.SDK.AuthTreeStore (AuthTreeState, eval, evalMergeScopes,
                                               initAuthTreeState)
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Errors        (AppError (..),
                                               SDKError (InternalError),
                                               throwSDKError)
import           Tendermint.SDK.Store         (ConnectionScope (..), IsKey (..),
                                               RawKey (..), RawStore, MergeScopes,
                                               StoreKey (..), delete, get, put, commitBlock, beginBlock, mergeScopes,
                                               withSandbox, withTransaction)
import           Test.Hspec
import Control.Monad (void)

spec :: Spec
spec = beforeAll beforeAction $
  describe "AuthTreeStore" $ do

    it "can fail to query an empty AuthTreeStore" $ \driver -> do
      Right mv <- runAuthTree driver $ get @'Query storeKey IntStoreKey
      mv `shouldBe` Nothing

    it "can set a value and query the value" $ \driver -> do
      Right mv <- runAuthTree driver $ do
        put @'Consensus storeKey IntStoreKey (IntStore 1)
        get @'Consensus storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes and roll back" $ \driver -> do
      Right mv'' <- runAuthTree driver $ do
        put @'Mempool storeKey IntStoreKey (IntStore 1)
        withSandbox $ do
          put storeKey IntStoreKey (IntStore 5)
          mv <- get storeKey IntStoreKey
          liftIO (mv `shouldBe` Just (IntStore 5))

          delete storeKey IntStoreKey
          mv' <- get storeKey IntStoreKey

          liftIO (mv' `shouldBe` Nothing)
        get @'Mempool storeKey IntStoreKey
      mv'' `shouldBe` Just (IntStore 1)

    it "can roll back if an error occurs during a transaction" $ \driver -> do
      Left apperr <- runAuthTree driver $ do
        put @'Consensus storeKey IntStoreKey (IntStore 1)
        withTransaction $ do
          put storeKey IntStoreKey (IntStore 6)
          throwSDKError InternalError
      appErrorCode apperr `shouldBe` 1
      Right mv <- runAuthTree driver $
        get @'Consensus storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)

    it "can make changes with a transaction" $ \driver -> do
      Right mv <- runAuthTree driver . withTransaction $ do
        put storeKey IntStoreKey (IntStore 5)
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 5)

    it "can merge the scopes" $ \driver -> do
      void $ runAuthTree driver . withTransaction $ do
        put @'Query storeKey IntStoreKey (IntStore 0)
        put @'Mempool storeKey IntStoreKey (IntStore 0)
        put @'Consensus storeKey IntStoreKey (IntStore 0)
        
        withSandbox $ (put :: _) storeKey IntStoreKey (IntStore 1)
        get @'Query storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
        get @'Mempool storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
        get @'Consensus storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)

        beginBlock

        withTransaction $ put storeKey IntStoreKey (IntStore 1)
        get @'Query storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
        get @'Mempool storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
        get @'Consensus storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)

        commitBlock

        get @'Query storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
        get @'Mempool storeKey IntStoreKey >>= liftIO . shouldBe (Just 0)
        get @'Consensus storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)

        mergeScopes
        get @'Query storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)
        get @'Mempool storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)
        get @'Consensus storeKey IntStoreKey >>= liftIO . shouldBe (Just 1)





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
  :: AuthTreeState
  -> Sem [ Tagged 'Query RawStore
         , Tagged 'Mempool RawStore
         , Tagged 'Consensus RawStore
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
  eval
