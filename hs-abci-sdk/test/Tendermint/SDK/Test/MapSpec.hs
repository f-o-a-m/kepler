module Tendermint.SDK.Test.MapSpec where

import           Control.Lens                             (iso)
import           Data.ByteString                          (ByteString)
import           Data.IORef                               (IORef, newIORef)
import           Data.String.Conversions                  (cs)
import           Data.Word                                (Word64)
import           Polysemy                                 (Embed, Sem, runM)
import           Polysemy.Error                           (Error, runError)
import qualified Tendermint.SDK.BaseApp                   as BA
import           Tendermint.SDK.BaseApp.Store             (Version (..))
import qualified Tendermint.SDK.BaseApp.Store.Map as M
import           Tendermint.SDK.BaseApp.Store.MemoryStore as Mem
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Test.Hspec

spec :: Spec
spec = pure ()

--------------------------------------------------------------------------------
-- Store types
--------------------------------------------------------------------------------

data Namespace

store :: BA.Store Namespace
store = BA.makeStore $ BA.KeyRoot "namespace"

newtype Key = Key Word64 deriving (Eq, Show, Num)

instance BA.RawKey Key where
    rawKey = iso (\(Key k) -> encode k)
      (either (const $ error "KeyError") Key . decode)

newtype Val = Val Word64 deriving (Eq, Show, Num, HasCodec)

data ValMapKey = ValMapKey

instance BA.RawKey ValMapKey where
    rawKey = iso (\_ -> cs valMapKey) (const ValMapKey)
      where
        valMapKey :: ByteString
        valMapKey =  cs $ ("valArray" :: String)

instance BA.IsKey ValMapKey Namespace where
  type Value ValMapKey Namespace = M.Map Key Val


valMap :: M.Map Key Val
valMap = M.makeMap ValMapKey store

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
