module Nameservice.Application
  ( AppConfig(..)
  , makeAppConfig
  , Handler
  , compileToBaseApp
  , runHandler
  , QueryApi
  , queryServer
  , router
  ) where

import           Data.Proxy
import qualified Nameservice.Modules.Nameservice  as N
import qualified Nameservice.Modules.Token        as T
import           Polysemy                         (Sem)
import           Servant.API                      ((:<|>) (..))
import qualified Tendermint.SDK.Auth              as A
import           Tendermint.SDK.BaseApp           ((:&))
import qualified Tendermint.SDK.BaseApp           as BaseApp
import           Tendermint.SDK.Crypto            (Secp256k1)
import qualified Tendermint.SDK.Logger.Katip      as KL
import           Tendermint.SDK.Module            (Modules (..))
import           Tendermint.SDK.Query             (QueryApplication, serve)
import qualified Tendermint.SDK.TxRouter          as R
import           Tendermint.SDK.Types.Transaction (RawTransaction)

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
  N.NameserviceEffR :& T.TokenEffR :& A.AuthEffR :& BaseApp.BaseApp

type Handler = Sem EffR

compileToBaseApp
  :: Sem EffR a
  -> Sem BaseApp.BaseApp a
compileToBaseApp = A.eval . T.eval . N.eval

-- NOTE: this should probably go in the library
runHandler
  :: AppConfig
  -> Handler a
  -> IO a
runHandler AppConfig{baseAppContext} =
  BaseApp.eval baseAppContext . compileToBaseApp

--------------------------------------------------------------------------------

modules :: Modules '[N.NameserviceM EffR] EffR
modules = ConsModule N.nameserviceModule NilModules

router :: RawTransaction -> Sem EffR ()
router = R.router (Proxy @Secp256k1) modules

--------------------------------------------------------------------------------

type QueryApi = T.Api :<|> N.Api

queryServer :: QueryApplication (Sem BaseApp.BaseApp)
queryServer = serve (Proxy :: Proxy QueryApi) (T.server :<|> N.server)
