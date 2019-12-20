module Nameservice.Application
  ( AppConfig(..)
  , makeAppConfig
  , EffR
  , NameserviceModules
  , handlersContext
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice           as N
import qualified Nameservice.Modules.Token                 as T
import           Tendermint.SDK.Application                (HandlersContext (..),
                                                            Modules (..))
import           Tendermint.SDK.BaseApp                    ((:&))
import qualified Tendermint.SDK.BaseApp                    as BaseApp
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus as Prometheus
import           Tendermint.SDK.Crypto                     (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth               as A

data AppConfig = AppConfig
  { baseAppContext :: BaseApp.Context
  }

makeAppConfig :: Prometheus.MetricsConfig -> KL.LogConfig -> IO AppConfig
makeAppConfig metCfg logCfg = do
  c <- BaseApp.makeContext (Just metCfg) logCfg
  pure $ AppConfig { baseAppContext = c
                   }

--------------------------------------------------------------------------------

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
  }
  where
  nameserviceModules :: Modules NameserviceModules EffR
  nameserviceModules =
       N.nameserviceModule
    :+ T.tokenModule
    :+ A.authModule
    :+ NilModules
