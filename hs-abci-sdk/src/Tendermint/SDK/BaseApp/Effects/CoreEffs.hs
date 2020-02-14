{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Effects.CoreEffs
  ( CoreEffs
  , Context(..)
  , contextLogConfig
  , contextPrometheusEnv
  , contextVersions
  , contextGrpcClient
  , makeContext
  , runCoreEffs
  ) where

import           Control.Lens                              (makeLenses)
import           Data.Text                                 (Text)
import qualified Katip                                     as K
import           Polysemy                                  (Embed, Sem, runM)
import           Polysemy.Reader                           (Reader, runReader)
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus as P
import qualified Tendermint.SDK.BaseApp.Store.IAVLStore    as IAVL

-- | CoreEffs is one level below BaseAppEffs, and provides one possible
-- | interpretation for its effects to IO.
type CoreEffs =
  '[ Reader KL.LogConfig
   , Reader (Maybe P.PrometheusEnv)
   , Reader IAVL.IAVLVersions
   , Reader IAVL.GrpcClient
   , Embed IO
   ]

-- | 'Context' is the environment required to run 'CoreEffs' to 'IO'
data Context = Context
  { _contextLogConfig     :: KL.LogConfig
  , _contextPrometheusEnv :: Maybe P.PrometheusEnv
  , _contextGrpcClient    :: IAVL.GrpcClient
  , _contextVersions      :: IAVL.IAVLVersions
  }

makeLenses ''Context

makeContext
  :: KL.InitialLogNamespace
  -> Maybe P.MetricsScrapingConfig
  -> IAVL.IAVLVersions
  -> IAVL.GrpcConfig
  -> IO Context
makeContext KL.InitialLogNamespace{..} scrapingCfg versions rpcConf = do
  metCfg <- case scrapingCfg of
        Nothing -> pure Nothing
        Just scfg -> P.emptyState >>= \es ->
          pure . Just $ P.PrometheusEnv es scfg
  logCfg <- mkLogConfig _initialLogEnvironment _initialLogProcessName
  grpc <- IAVL.initGrpcClient rpcConf
  pure $ Context
    { _contextLogConfig = logCfg
    , _contextPrometheusEnv = metCfg
    , _contextVersions = versions
    , _contextGrpcClient = grpc
    }
    where
      mkLogConfig :: Text -> Text -> IO KL.LogConfig
      mkLogConfig env pName = do
        let mkLogEnv = K.initLogEnv (K.Namespace [pName]) (K.Environment env)
        le <- mkLogEnv
        return $ KL.LogConfig
          { _logNamespace = mempty
          , _logContext = mempty
          , _logEnv = le
          }

-- | The standard interpeter for 'CoreEffs'.
runCoreEffs
  :: Context
  -> forall a. Sem CoreEffs a -> IO a
runCoreEffs Context{..} =
  runM .
    runReader _contextGrpcClient .
    runReader _contextVersions .
    runReader _contextPrometheusEnv .
    runReader _contextLogConfig
