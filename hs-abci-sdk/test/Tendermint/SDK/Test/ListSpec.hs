module Tendermint.SDK.Test.ListSpec (spec) where

import           Control.Lens                             (iso)
import           Data.ByteString                          (ByteString)
import           Data.IORef                               (IORef, newIORef)
import           Data.String.Conversions                  (cs)
import           Data.Word                                (Word64)
import           Polysemy                                 (Embed, Sem, runM)
import           Polysemy.Error                           (Error, runError)
import qualified Tendermint.SDK.BaseApp                   as BA
import           Tendermint.SDK.BaseApp.Store             (Version (..))
import qualified Tendermint.SDK.BaseApp.Store.List        as L
import           Tendermint.SDK.BaseApp.Store.MemoryStore as Mem
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Test.Hspec

spec :: Spec
spec =

  beforeAll makeConfig $
    describe "List Spec" $ do

      it "Can create an empty list" $ \config -> do
        res <- runToIO config $ L.toList valList
        res `shouldBe` []

      it "Can add an element to the list then delete it" $ \config -> do
        lInit <- runToIO config $ L.toList valList
        lInit `shouldBe` []
        let n = 1
        res <- runToIO config $ do
          L.append n valList
          mi <- L.elemIndex n valList
          me <- maybe (pure Nothing) (\i -> valList L.!! i) mi 
          l <- L.toList valList
          len <- L.length valList
          pure (mi, me , l, len)
        res `shouldBe` (Just 0, Just n, [n], 1)
        res' <- runToIO config $ do
          L.delete n valList
          i <- L.elemIndex n valList
          len <- L.length valList
          l <- L.toList valList
          pure (i, len, l)
        res' `shouldBe` (Nothing, 0, [])
        runToIO config $ L.deleteWhen (const True) valList

      it "Can add two elements and delete the head" $ \config -> do
        lInit <- runToIO config $ L.toList valList
        lInit `shouldBe` []
        let n = 1
            m = 2
        res <- runToIO config $ do
          L.append n valList
          L.append m valList
          mi <- L.elemIndex m valList
          me <- maybe (pure Nothing) (\i -> valList L.!! i) mi 
          l <- L.toList valList
          len <- L.length valList
          pure (mi, me, l, len)
        res `shouldBe` (Just 0, Just m, [m,n], 2)
        res' <- runToIO config $ do
          L.delete m valList
          i <- L.elemIndex m valList
          len <- L.length valList
          l <- L.toList valList
          pure (i, len, l)
        res' `shouldBe` (Nothing, 1, [n])
        runToIO config $ L.deleteWhen (const True) valList

      it "Can add add three elements and delete the second" $ \config -> do
        lInit <- runToIO config $ L.toList valList
        lInit `shouldBe` []
        let n = 1
            m = 2
            k = 3
        res <- runToIO config $ do
          L.append n valList
          L.append m valList
          L.append k valList
          mi <- L.elemIndex m valList
          me <- maybe (pure Nothing) (\i -> valList L.!! i) mi 
          l <- L.toList valList
          len <- L.length valList
          pure (mi, me, l, len)
        res `shouldBe` (Just 1, Just m, [k,m,n], 3)
        res' <- runToIO config $ do
          L.delete m valList
          i <- L.elemIndex m valList
          len <- L.length valList
          l <- L.toList valList
          pure (i, len, l)
        res' `shouldBe` (Nothing, 2, [k,n])
        runToIO config $ L.deleteWhen (const True) valList

      it "Can add add three elements and delete the third" $ \config -> do
        lInit <- runToIO config $ L.toList valList
        lInit `shouldBe` []
        let n = 1
            m = 2
            k = 3
        res <- runToIO config $ do
          L.append n valList
          L.append m valList
          L.append k valList
          mi <- L.elemIndex n valList
          me <- maybe (pure Nothing) (\i -> valList L.!! i)  mi 
          l <- L.toList valList
          len <- L.length valList
          pure (mi, me, l, len)
        res `shouldBe` (Just 2, Just n, [k,m,n], 3)
        res' <- runToIO config $ do
          L.delete n valList
          i <- L.elemIndex n valList
          len <- L.length valList
          l <- L.toList valList
          pure (i, len, l)
        res' `shouldBe` (Nothing, 2, [k,m])
        runToIO config $ L.deleteWhen (const True) valList









--------------------------------------------------------------------------------
-- Store types
--------------------------------------------------------------------------------

data Namespace

store :: BA.Store Namespace
store = BA.makeStore $ BA.KeyRoot "namespace"

data ValListKey = ValListKey

instance BA.RawKey ValListKey where
    rawKey = iso (\_ -> cs valListKey) (const ValListKey)
      where
        valListKey :: ByteString
        valListKey =  cs $ ("valArray" :: String)

instance BA.IsKey ValListKey Namespace where
  type Value ValListKey Namespace = L.List Val

newtype Val = Val Word64 deriving (Eq, Show, Num, HasCodec)

valList :: L.List Val
valList = L.makeList ValListKey store

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
