module Tendermint.SDK.Test.AuthTreeStoreSpec where

import           Control.Lens                 (iso)
import qualified Data.Binary                  as Binary
import           Data.ByteString              (ByteString)
import           Data.String.Conversions      (cs)
import           Polysemy                     (runM)
import           Tendermint.SDK.AuthTreeStore (AuthTreeDriver,
                                               initAuthTreeDriver,
                                               interpretAuthTreeStore)
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Store         (IsKey (..), RawKey (..),
                                               StoreKey (..), get, put)
import           Test.Hspec

spec :: Spec
spec = beforeAll beforeAction $
  describe "AuthTreeStore" $ do
    it "can fail to query an empty AuthTreeStore" $ \driver -> do
      mv <- runM . interpretAuthTreeStore driver $ get storeKey IntStoreKey
      mv `shouldBe` Nothing
    it "can set a value and query the value" $ \driver -> do
      mv <- runM . interpretAuthTreeStore driver $ do
        put storeKey IntStoreKey (IntStore 1)
        get storeKey IntStoreKey
      mv `shouldBe` Just (IntStore 1)


beforeAction :: IO AuthTreeDriver
beforeAction = initAuthTreeDriver

newtype IntStore = IntStore Int deriving (Eq, Show)

data IntStoreKey = IntStoreKey

instance HasCodec IntStore where
    encode (IntStore c) = cs . Binary.encode $ c
    decode = Right . IntStore . Binary.decode . cs

instance RawKey IntStoreKey where
    rawKey = iso (\_ -> cs intStoreKey) (const IntStoreKey)
      where
        intStoreKey :: ByteString
        intStoreKey = "IntStore"

instance IsKey IntStoreKey "int_store" where
    type Value IntStoreKey "int_store" = IntStore

storeKey :: StoreKey "int_store"
storeKey = StoreKey "int_store"
