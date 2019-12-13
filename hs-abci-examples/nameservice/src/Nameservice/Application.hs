module Nameservice.Application
  ( AppConfig(..)
  , makeAppConfig
  , EffR
  , NameserviceModules
  , handlersContext
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice     as N
import qualified Nameservice.Modules.Token           as T
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
  c <- BaseApp.makeContext logCfg
  pure $ AppConfig { baseAppContext = c
                   }

--------------------------------------------------------------------------------

type EffR =
   N.NameserviceEffs BaseApp.:&
   T.TokenEffs BaseApp.:&
   A.AuthEffs BaseApp.:&
   BaseApp.BaseApp BaseApp.CoreEffs

type NameserviceModules = '[N.NameserviceM EffR, T.TokenM EffR, A.AuthM EffR]

handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR BaseApp.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToCore  = BaseApp.compileScopedEff
  }
  where
  nameserviceModules :: Modules NameserviceModules EffR
  nameserviceModules =
    ConsModule N.nameserviceModule $
      ConsModule T.tokenModule $
      ConsModule A.authModule NilModules
