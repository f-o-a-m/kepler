module Nameservice.Application
  ( AppConfig(..)
  , makeAppConfig
  , Handler
  , compileToBaseApp
  , runHandler
  , queryServer
  ) where

import           Control.Exception               (Exception)
import           Data.Proxy
import qualified Nameservice.Modules.Nameservice as N
import qualified Nameservice.Modules.Token       as T
import           Polysemy                        (Sem)
import           Servant.API                     ((:<|>) (..))
import qualified Tendermint.SDK.Auth             as A
import           Tendermint.SDK.BaseApp          ((:&))
import qualified Tendermint.SDK.BaseApp          as BaseApp
import qualified Tendermint.SDK.Logger.Katip     as KL
import           Tendermint.SDK.Query            (QueryApplication, serve)

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

type QueryApi = T.Api :<|> N.Api

apiP :: Proxy QueryApi
apiP = Proxy

queryServer :: QueryApplication (Sem BaseApp.BaseApp)
queryServer = serve (Proxy :: Proxy QueryApi) (T.server :<|> N.server)
