module Nameservice.Application
  ( EffR
  , NameserviceModules
  , handlersContext
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice as N
import qualified Nameservice.Modules.Token       as T
import           Tendermint.SDK.Application      (HandlersContext (..),
                                                  Modules (..),
                                                  baseAppAnteHandler)
import           Tendermint.SDK.BaseApp          ((:&))
import qualified Tendermint.SDK.BaseApp          as BaseApp
import           Tendermint.SDK.Crypto           (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth     as A

type EffR =
   N.NameserviceEffs :&
   T.TokenEffs :&
   A.AuthEffs :&
   BaseApp.BaseApp BaseApp.CoreEffs

type NameserviceModules =
   '[ N.NameserviceM EffR
    , T.TokenM EffR
    , A.AuthM EffR
    ]

handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR BaseApp.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToCore  = BaseApp.compileScopedEff
  , anteHandlers = baseAppAnteHandler
  }
  where
  nameserviceModules :: Modules NameserviceModules EffR
  nameserviceModules =
       N.nameserviceModule
    :+ T.tokenModule
    :+ A.authModule
    :+ NilModules
