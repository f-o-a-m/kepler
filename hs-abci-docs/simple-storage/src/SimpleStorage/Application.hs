module SimpleStorage.Application
  ( EffR
  , SimpleStorageModules
  , handlersContext
  ) where

import           Data.Proxy
import           SimpleStorage.Modules.SimpleStorage as SimpleStorage
import           Tendermint.SDK.Application          (HandlersContext (..),
                                                      ModuleList (..),
                                                      baseAppAnteHandler)
import           Tendermint.SDK.BaseApp              ((:&))
import qualified Tendermint.SDK.BaseApp              as BA
import           Tendermint.SDK.Crypto               (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth         as A

--------------------------------------------------------------------------------

type EffR =
  SimpleStorage.SimpleStorageEffs :&
  A.AuthEffs


type SimpleStorageModules =
  '[ SimpleStorage.SimpleStorage
   , A.Auth
   ]

handlersContext :: HandlersContext Secp256k1 SimpleStorageModules EffR BA.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = simpleStorageModules
  , compileToCore  = BA.defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
  simpleStorageModules :: ModuleList SimpleStorageModules (BA.AppEffs EffR BA.CoreEffs)
  simpleStorageModules =
       SimpleStorage.simpleStorageModule
    :+ A.authModule
    :+ NilModules
