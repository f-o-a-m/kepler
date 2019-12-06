module Nameservice.Application
  ( AppConfig(..)
  , makeAppConfig
  , EffR
  , NameserviceModules
  , handlersContext
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice as N
import qualified Nameservice.Modules.Token       as T
import           Polysemy                        (Sem)
import qualified Tendermint.SDK.Auth             as A
import           Tendermint.SDK.BaseApp          ((:&))
import qualified Tendermint.SDK.BaseApp          as BaseApp
import           Tendermint.SDK.Crypto           (Secp256k1)
import           Tendermint.SDK.Handlers
import qualified Tendermint.SDK.Logger.Katip     as KL
import           Tendermint.SDK.Module           (Modules (..))

data AppConfig = AppConfig
  { baseAppContext :: BaseApp.Context
  }

makeAppConfig :: KL.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  c <- BaseApp.makeContext logCfg
  pure $ AppConfig { baseAppContext = c
                   }

--------------------------------------------------------------------------------

type EffR = N.NameserviceEffs :& T.TokenEffs :& A.AuthEffs :& BaseApp.BaseApp BaseApp.CoreEffs

type NameserviceModules = '[T.TokenM EffR, N.NameserviceM EffR]

handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR BaseApp.CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToBaseApp = compileNameserviceToBaseApp
  , compileToCore  = BaseApp.compileScopedEff
  }
  where
  nameserviceModules :: Modules NameserviceModules EffR
  nameserviceModules = ConsModule T.tokenModule $ ConsModule N.nameserviceModule NilModules

  compileNameserviceToBaseApp :: Sem EffR a -> Sem (BaseApp.BaseApp BaseApp.CoreEffs) a
  compileNameserviceToBaseApp = A.eval . T.eval . N.eval
