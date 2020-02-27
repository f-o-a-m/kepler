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
    describe "StoreList Spec" $

      it "Can create an empty list" $ \config -> do
        res <- runToIO config $ L.toList valList
        res `shouldBe` []




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
        valListKey =  cs $ ("valList" :: String)

newtype Val = Val Word64 deriving (Eq, Show, Num, HasCodec)

valList :: L.StoreList Val
valList = L.makeStoreList ValListKey store

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
