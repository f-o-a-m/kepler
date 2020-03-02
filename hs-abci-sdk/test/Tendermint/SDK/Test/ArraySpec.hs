module Tendermint.SDK.Test.ArraySpec (spec) where

import           Control.Lens                             (iso)
import           Data.ByteString                          (ByteString)
import           Data.IORef                               (IORef, newIORef)
import           Data.Maybe                               (fromJust, isJust)
import           Data.String.Conversions                  (cs)
import           Data.Word                                (Word64)
import           Polysemy                                 (Embed, Sem, runM)
import           Polysemy.Error                           (Error, runError)
import qualified Tendermint.SDK.BaseApp                   as BA
import           Tendermint.SDK.BaseApp.Store             (Version (..))
import qualified Tendermint.SDK.BaseApp.Store.Array       as A
import           Tendermint.SDK.BaseApp.Store.MemoryStore as Mem
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Test.Hspec

spec :: Spec
spec =
  beforeAll makeConfig $
    describe "Array Spec" $ do

      it "Can create an empty array" $ \config -> do
        res <- runToIO config $ A.toList valArray
        res `shouldBe` []

      it "Can add an element to the array" $ \config -> do
        res <- runToIO config $ do
          let n = 1
          A.append n valArray
          mi <- A.elemIndex n valArray
          l <- A.toList valArray
          pure (mi, l)
        res `shouldBe` (Just 0, [1])
        runToIO config $ A.deleteWhen (const True) valArray

      it "Can add an element, modify it, then delete it" $ \config -> do
        let n = 2
            m = 3

        -- save the element and get its index
        i <- runToIO config $ do
          A.append n valArray
          A.elemIndex n valArray
        i `shouldSatisfy` isJust

        -- accessing at the index gets the value back again
        n' <- runToIO config (valArray A.!! fromJust i)
        Just n `shouldBe` n'

        -- modifying the element at the index is successful
        mm <- runToIO config $ A.modifyAtIndex (fromJust i) (const m) valArray
        mm `shouldBe` Just m

        -- deleting the element and trying to find it gives Nothing
        res2 <- runToIO config $ do
          A.deleteWhen (== m) valArray
          A.elemIndex n valArray
        res2 `shouldBe` Nothing

        -- modifying a deleted element gives Nothing
        let k = 4
        mm' <- runToIO config $ A.modifyAtIndex (fromJust i) (const k) valArray
        mm' `shouldBe` Nothing





--------------------------------------------------------------------------------
-- Store types
--------------------------------------------------------------------------------

data Namespace

store :: BA.Store Namespace
store = BA.makeStore $ BA.KeyRoot "namespace"

data ValArrayKey = ValArrayKey

instance BA.RawKey ValArrayKey where
    rawKey = iso (\_ -> cs valArrayKey) (const ValArrayKey)
      where
        valArrayKey :: ByteString
        valArrayKey =  cs $ ("valArray" :: String)

instance BA.IsKey ValArrayKey Namespace where
  type Value ValArrayKey Namespace = A.Array Val

newtype Val = Val Word64 deriving (Eq, Show, Num, HasCodec)

valArray :: A.Array Val
valArray = A.makeArray ValArrayKey store

--------------------------------------------------------------------------------
-- Interpreter
--------------------------------------------------------------------------------

type Effs =
  [ BA.ReadStore
  , BA.WriteStore
  , Error BA.AppError
  , Embed IO
  ]

data Config = Config
  { configDB      :: Mem.DB
  , configVersion :: IORef Version
  }

makeConfig :: IO Config
makeConfig = do
  db <- Mem.initDB
  v <- newIORef Latest
  pure $ Config db v

runToIO
  :: Config
  -> forall a.
     Sem Effs a
  -> IO a
runToIO Config{configDB, configVersion} m = do
  eRes <-
    runM .
      runError .
      evalWrite configDB .
      evalRead configDB configVersion $ m
  either (error . show) pure eRes
