module SimpleStorage.Application
  ( SimpleStorageModules
  , handlersContext
  ) where

import           Data.Proxy
import           SimpleStorage.Modules.SimpleStorage as SimpleStorage
import           Tendermint.SDK.Application          (HandlersContext (..),
                                                      ModuleList (..),
                                                      baseAppAnteHandler)
import qualified Tendermint.SDK.BaseApp              as BA
import           Tendermint.SDK.Crypto               (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth         as A

--------------------------------------------------------------------------------

type SimpleStorageModules =
  '[ SimpleStorage.SimpleStorage
   , B.Bank
   , A.Auth
   ]

handlersContext :: HandlersContext Secp256k1 SimpleStorageModules BA.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = simpleStorageModules
  , compileToCore  = BA.defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
  simpleStorageModules =
       SimpleStorage.simpleStorageModule
    :+ B.bankModule
    :+ A.authModule
    :+ NilModules
