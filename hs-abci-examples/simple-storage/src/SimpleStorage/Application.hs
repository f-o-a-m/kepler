module SimpleStorage.Application
  ( AppConfig(..)
  , makeAppConfig
  , SimpelStorageEffs
  , handlersContext
  ) where

import           Data.Proxy
import           Polysemy                            (Sem)
import           SimpleStorage.Modules.SimpleStorage as SimpleStorage
import           Tendermint.SDK.Application          (HandlersContext (..),
                                                      Modules (..))
import qualified Tendermint.SDK.BaseApp              as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip as KL
import           Tendermint.SDK.Crypto               (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth         as A

data AppConfig = AppConfig
  { baseAppContext :: BaseApp.Context
  }

makeAppConfig :: KL.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  c <- BaseApp.makeContext Nothing logCfg
  pure $ AppConfig { baseAppContext = c
                   }

--------------------------------------------------------------------------------

type SimpelStorageEffs =
  SimpleStorage.SimpleStorage ':  A.AuthEffs BaseApp.:& BaseApp.BaseApp BaseApp.CoreEffs

type SimpleStorageModules =
  '[SimpleStorage.SimpleStorageM SimpelStorageEffs]

handlersContext :: HandlersContext Secp256k1 SimpleStorageModules SimpelStorageEffs BaseApp.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = simpleStorageModules
  , compileToBaseApp = compileSimpleStorageToBaseApp
  , compileToCore  = BaseApp.compileScopedEff
  }
  where
  simpleStorageModules :: Modules SimpleStorageModules SimpelStorageEffs
  simpleStorageModules = ConsModule SimpleStorage.simpleStorageModule NilModules

  compileSimpleStorageToBaseApp :: Sem SimpelStorageEffs a -> Sem (BaseApp.BaseApp BaseApp.CoreEffs) a
  compileSimpleStorageToBaseApp = A.eval . SimpleStorage.eval
