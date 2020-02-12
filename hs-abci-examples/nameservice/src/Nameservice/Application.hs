module Nameservice.Application
  ( EffR
  , NameserviceModules
  , handlersContext
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice as N
import qualified Nameservice.Modules.Token       as T
import           Tendermint.SDK.Application      (BaseApp, HandlersContext (..),
                                                  ModuleList (..),
                                                  baseAppAnteHandler,
                                                  defaultCompileToCore)
import           Tendermint.SDK.BaseApp          ((:&))
import qualified Tendermint.SDK.BaseApp          as BaseApp
import           Tendermint.SDK.Crypto           (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth     as A

type EffR =
   N.NameserviceEffs :&
   T.TokenEffs :&
   A.AuthEffs :&
   BaseApp.TxEffs :&
   BaseApp BaseApp.CoreEffs

type NameserviceModules =
   '[ N.NameserviceM EffR
    , T.TokenM EffR
    , A.AuthM EffR
    ]

handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR BaseApp.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToCore  = defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
  nameserviceModules :: ModuleList NameserviceModules EffR
  nameserviceModules =
       N.nameserviceModule
    :+ T.tokenModule
    :+ A.authModule
    :+ NilModules
