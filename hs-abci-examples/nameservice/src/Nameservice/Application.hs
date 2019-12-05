module Nameservice.Application
  ( AppConfig(..)
  , makeAppConfig
  , Handler
  , compileToBaseApp
  --, runHandler
  , QueryApi
  , queryServer
  , router
  ) where

import           Data.ByteString                 (ByteString)
import           Data.Proxy
import qualified Nameservice.Modules.Nameservice as N
import qualified Nameservice.Modules.Token       as T
import           Polysemy                        (Sem)
import           Servant.API                     ((:<|>) (..))
import qualified Tendermint.SDK.Auth             as A
import           Tendermint.SDK.BaseApp          ((:&))
import qualified Tendermint.SDK.BaseApp          as BaseApp
import           Tendermint.SDK.Crypto           (Secp256k1)
import qualified Tendermint.SDK.Logger.Katip     as KL
import           Tendermint.SDK.Module           (Module (..), Modules (..))
import           Tendermint.SDK.Query            (QueryApplication, hoistRoute,
                                                  serve)
import qualified Tendermint.SDK.TxRouter         as R

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
  N.NameserviceEffs :& T.TokenEffs :& A.AuthEffs :& BaseApp.BaseApp


compileToBaseApp
  :: Sem EffR a
  -> Sem BaseApp.BaseApp a
compileToBaseApp = A.eval . T.eval . N.eval

type Handler = Sem EffR

-- NOTE: this should probably go in the library
--runHandler
--  :: AppConfig
--  -> Handler a
--  -> IO a
--runHandler AppConfig{baseAppContext} =
--  BaseApp.eval baseAppContext . compileToBaseApp

--------------------------------------------------------------------------------

modules :: Modules '[T.TokenM EffR ,N.NameserviceM EffR] EffR
modules = ConsModule T.tokenModule $ ConsModule N.nameserviceModule NilModules

type QueryApi = T.Api :<|> N.Api

router
  :: ByteString
  -> Sem BaseApp.BaseApp ()
router = compileToBaseApp . R.router (Proxy @Secp256k1) modules

queryServer :: QueryApplication (Sem BaseApp.BaseApp)
queryServer =
  let queryRouter = hoistRoute (Proxy :: Proxy QueryApi) compileToBaseApp
        (moduleQueryServer T.tokenModule :<|> moduleQueryServer N.nameserviceModule)
  in serve (Proxy :: Proxy QueryApi) queryRouter
