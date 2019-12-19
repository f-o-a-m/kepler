module Nameservice.Application
  ( AppConfig(..)
  , makeAppConfig
  , EffR
  , NameserviceModules
  , handlersContext
  , addScribesToLogEnv
  ) where

import           Control.Arrow                                (Kleisli (..),
                                                               returnA, (>>>))
import           Control.Concurrent.MVar                      (MVar, newMVar)
import           Control.Error                                (MaybeT (..),
                                                               runMaybeT)
import           Data.Map.Strict                              (Map, empty)
import           Data.Maybe                                   (fromMaybe)
import           Data.Proxy
import           Data.String.Conversions                      (cs)
import           Data.Text                                    (Text)
import qualified Database.V5.Bloodhound                       as BH
import qualified Katip                                        as K
import qualified Katip.Scribes.Datadog.TCP                    as DD
import qualified Katip.Scribes.ElasticSearch                  as ES
import qualified Nameservice.Modules.Nameservice              as N
import qualified Nameservice.Modules.Token                    as T
import           Network.ABCI.Server.Middleware.MetricsLogger (OrderedMessageType)
import qualified Network.HTTP.Client                          as Client
import           System.Environment
import           System.IO                                    (stdout)
import           Tendermint.SDK.Application                   (HandlersContext (..),
                                                               Modules (..))
import           Tendermint.SDK.BaseApp                       ((:&))
import qualified Tendermint.SDK.BaseApp                       as BaseApp
import           Tendermint.SDK.BaseApp.Logger.Katip          as KL
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus    as P
import           Tendermint.SDK.Crypto                        (Secp256k1)
import qualified Tendermint.SDK.Modules.Auth                  as A
import           Text.Read                                    (read)


data KatipConfig = ES {host :: String, port :: String} | Console

makeConsoleLoggingConfig :: IO KatipConfig
makeConsoleLoggingConfig = do
  mEsConfig <- runMaybeT $
    ES <$> (MaybeT $ lookupEnv "ES_HOST") <*> (MaybeT $ lookupEnv "ES_PORT")
  pure $ fromMaybe Console mEsConfig

-- makes a log environment for console logs / ES logs
makeKatipScribe :: KatipConfig -> Kleisli IO K.LogEnv K.LogEnv
makeKatipScribe kcfg = Kleisli $ \le ->
  let verbosity = K.V0
  in case kcfg of
    Console -> do
      handleScribe <- K.mkHandleScribe K.ColorIfTerminal stdout (K.permitItem K.DebugS) verbosity
      K.registerScribe "stdout" handleScribe K.defaultScribeSettings le
    ES {host, port} -> do
      mgr <- Client.newManager Client.defaultManagerSettings
      let serverAddress = "http://" <> host <> ":" <> port
          bloodhoundEnv = BH.mkBHEnv (BH.Server $ cs serverAddress) mgr
      esScribe <- ES.mkEsScribe ES.defaultEsScribeCfgV5 bloodhoundEnv (BH.IndexName "nameservice")
        (BH.MappingName "application-logs") (K.permitItem K.DebugS) verbosity
      K.registerScribe "es" esScribe K.defaultScribeSettings le

-- makes a log environment for metrics logs
makeMetricsScribe :: Text -> Kleisli IO K.LogEnv K.LogEnv
makeMetricsScribe key = Kleisli $ \le -> do
  let apiKey = DD.APIKey key
  datadogScribeSettings <- DD.mkDatadogScribeSettings DD.directAPIConnectionParams (DD.DirectAuth apiKey)
  scribe <- DD.mkDatadogScribe datadogScribeSettings (K.permitItem K.InfoS) K.V0
  K.registerScribe "datadog" scribe K.defaultScribeSettings le

addScribesToLogEnv :: AppConfig -> K.LogEnv -> IO K.LogEnv
addScribesToLogEnv AppConfig{..} initialLogEnv = do
  consoleCfg <- makeConsoleLoggingConfig
  let mdatadogApiKey =  P.dataDogApiKey . P.envMetricsScrapingConfig <$>
        BaseApp.contextPrometheusEnv baseAppContext
      addScribes = runKleisli $
        makeKatipScribe consoleCfg >>> maybe returnA makeMetricsScribe mdatadogApiKey
  addScribes initialLogEnv


data AppConfig = AppConfig
  { baseAppContext   :: BaseApp.Context
  , consoleLogConfig :: KatipConfig
  , serverMetricsMap :: MVar (Map OrderedMessageType Integer)
  }

makeAppConfig :: IO AppConfig
makeAppConfig = do
  prometheusEnv <- runMaybeT $ do
    prometheusPort <- read <$> MaybeT (lookupEnv "STATS_PORT")
    ddApiKey <- cs <$> MaybeT (lookupEnv "DD_API_KEY")
    pure $ P.MetricsScrapingConfig prometheusPort ddApiKey
  consoleCfg <- makeConsoleLoggingConfig
  serverMetricsMap <- newMVar empty
  c <- BaseApp.makeContext (KL.InitialLogNamespace "dev" "simple-storage") prometheusEnv
  pure $ AppConfig { baseAppContext = c
                   , consoleLogConfig = consoleCfg
                   , serverMetricsMap = serverMetricsMap
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
    ConsModule N.nameserviceModule $
      ConsModule T.tokenModule $
      ConsModule A.authModule $
      NilModules
