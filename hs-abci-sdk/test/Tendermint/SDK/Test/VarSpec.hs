module Tendermint.SDK.Test.VarSpec where

import           Control.Lens                             (iso)
import           Data.ByteString                          (ByteString)
import           Data.IORef                               (IORef, newIORef)
import           Data.String.Conversions                  (cs)
import           Data.Word                                (Word64)
import           Polysemy                                 (Embed, Sem, runM)
import           Polysemy.Error                           (Error, runError)
import qualified Tendermint.SDK.BaseApp                   as BA
import           Tendermint.SDK.BaseApp.Store             (Version (..))
import           Tendermint.SDK.BaseApp.Store.MemoryStore as Mem
import qualified Tendermint.SDK.BaseApp.Store.Var         as V
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Test.Hspec

spec :: Spec
spec =
  beforeAll makeConfig $
    describe "Var Spec" $ do

      it "Can create an empty var" $ \config -> do
        res <- runToIO config $ V.takeVar valVar
        res `shouldBe` Nothing

      it "Can put a val in a var and take it out" $ \config -> do
        res <- runToIO config $ do
          V.putVar 1 valVar
          v <- V.takeVar valVar
          v' <- V.unsafeTakeVar valVar
          pure (v, v')
        res `shouldBe` (Just 1, 1)
        runToIO config $ V.deleteVar valVar


--------------------------------------------------------------------------------
-- Store types
--------------------------------------------------------------------------------

data Namespace

store :: BA.Store Namespace
store = BA.makeStore $ BA.KeyRoot "namespace"

data ValVarKey = ValVarKey

instance BA.RawKey ValVarKey where
    rawKey = iso (\_ -> cs valVarKey) (const ValVarKey)
      where
        valVarKey :: ByteString
        valVarKey =  cs $ ("valVar" :: String)

instance BA.IsKey ValVarKey Namespace where
  type Value ValVarKey Namespace = V.Var Val

newtype Val = Val Word64 deriving (Eq, Show, Num, HasCodec)

valVar :: V.Var Val
valVar = V.makeVar ValVarKey store


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
