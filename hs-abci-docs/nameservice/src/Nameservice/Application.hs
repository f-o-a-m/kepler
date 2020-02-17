module Nameservice.Application
  ( EffR
  , NameserviceModules
  , handlersContext
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice as N
import           Tendermint.SDK.Application      (HandlersContext (..),
                                                  ModuleList (..),
                                                  baseAppAnteHandler)
import qualified Tendermint.SDK.BaseApp          as BA
import           Tendermint.SDK.Crypto           (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth     as A
import qualified Tendermint.SDK.Modules.Bank     as B

type EffR =
   N.NameserviceEffs BA.:&
   B.BankEffs BA.:&
   A.AuthEffs BA.:&
   BA.TxEffs BA.:&
   BA.BaseApp BA.CoreEffs

type NameserviceModules =
   '[ N.NameserviceM EffR
    , B.BankM EffR
    , A.AuthM EffR
    ]

handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR BA.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToCore  = BA.defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
  nameserviceModules :: ModuleList NameserviceModules EffR
  nameserviceModules =
       N.nameserviceModule
    :+ B.bankModule
    :+ A.authModule
    :+ NilModules
