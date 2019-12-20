module SimpleStorage.Application
  ( EffR
  , SimpleStorageModules
  , handlersContext
  ) where

import           Data.Proxy
import           SimpleStorage.Modules.SimpleStorage as SimpleStorage
import           Tendermint.SDK.Application          (HandlersContext (..),
                                                      Modules (..))
import           Tendermint.SDK.BaseApp              ((:&))
import qualified Tendermint.SDK.BaseApp              as BaseApp
import           Tendermint.SDK.Crypto               (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth         as A

--------------------------------------------------------------------------------

type EffR =
  SimpleStorage.SimpleStorageEffs :&
  A.AuthEffs :&
  BaseApp.BaseApp BaseApp.CoreEffs

type SimpleStorageModules =
  '[ SimpleStorage.SimpleStorageM EffR
   , A.AuthM EffR
   ]

handlersContext :: HandlersContext Secp256k1 SimpleStorageModules EffR BaseApp.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = simpleStorageModules
  , compileToCore  = BaseApp.compileScopedEff
  }
  where
  simpleStorageModules :: Modules SimpleStorageModules EffR
  simpleStorageModules =
    ConsModule SimpleStorage.simpleStorageModule $
      ConsModule A.authModule $
      NilModules
