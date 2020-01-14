module Nameservice.Application
  ( EffR
  , NameserviceModules
  , handlersContext
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice as N
import           Tendermint.SDK.Application      (HandlersContext (..),
                                                  Modules (..),
                                                  baseAppAnteHandler)
import           Tendermint.SDK.BaseApp          ((:&))
import qualified Tendermint.SDK.BaseApp          as BaseApp
import           Tendermint.SDK.Crypto           (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth     as A
import qualified Tendermint.SDK.Modules.Bank     as B

type EffR =
   N.NameserviceEffs :&
   B.BankEffs :&
   A.AuthEffs :&
   BaseApp.BaseApp BaseApp.CoreEffs

type NameserviceModules =
   '[ N.NameserviceM EffR
    , B.BankM EffR
    , A.AuthM EffR
    ]

handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR BaseApp.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToCore  = BaseApp.compileScopedEff
  , anteHandler = baseAppAnteHandler
  }
  where
  nameserviceModules :: Modules NameserviceModules EffR
  nameserviceModules =
       N.nameserviceModule
    :+ B.bankModule
    :+ A.authModule
    :+ NilModules
