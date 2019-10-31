module Tendermint.SDK.Test.AuthTreeStoreSpec where

import           Control.Lens                 (iso)
import qualified Data.Binary                  as Binary
import           Data.ByteString              (ByteString)
import           Data.String.Conversions      (cs)
import           Polysemy

import           Tendermint.SDK.AuthTreeStore
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Store
import           Test.Hspec

spec :: Spec
spec = beforeAll beforeAction $
  describe "AuthTreeStore" $ do
    it "can fail to query an empty AuthTreeStore" $ \driver -> do
      mv <- runM . interpretAuthTreeStore driver $ get (undefined :: Root) IntStoreKey
      mv `shouldBe` Nothing
    it "can set a value and query the value" $ \driver -> do
      mv <- runM . interpretAuthTreeStore driver $ do
        put IntStoreKey (IntStore 1)
        get (undefined :: Root) IntStoreKey
      mv `shouldBe` Just (IntStore 1)


beforeAction :: IO AuthTreeDriver
beforeAction = initAuthTreeDriver

newtype IntStore = IntStore Int deriving (Eq, Show)

data IntStoreKey = IntStoreKey

instance HasCodec IntStore where
    encode (IntStore c) = cs . Binary.encode $ c
    decode = Right . IntStore . Binary.decode . cs

instance HasKey IntStore where
    type Key IntStore = IntStoreKey
    rawKey = iso (\_ -> cs intStoreKey) (const IntStoreKey)
      where
        intStoreKey :: ByteString
        intStoreKey = "IntStore"
