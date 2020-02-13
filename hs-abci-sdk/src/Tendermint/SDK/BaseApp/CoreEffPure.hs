{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp.CoreEffPure
  ( CoreEffsPure
  , PureContext(..)
  , contextLogConfig
  , contextPrometheusEnv
  , contextVersions
  , contextDB
  , makePureContext
  , runCoreEffsPure
  ) where

import           Control.Lens                              (makeLenses)
import           Data.Text                                 (Text)
import qualified Katip                                     as K
import           Polysemy                                  (Embed, Sem, runM)
import           Polysemy.Error                            (Error, runError)
import           Polysemy.Reader                           (Reader, runReader)
import           Tendermint.SDK.BaseApp.Errors             (AppError)
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus as P
import qualified Tendermint.SDK.BaseApp.Store.MemoryStore  as Memory

-- | CoreEffs is one level below BaseAppEffs, and provides one possible
-- | interpretation for its effects to IO.
type CoreEffsPure =
  '[ Reader KL.LogConfig
   , Reader (Maybe P.PrometheusEnv)
   , Reader Memory.DBVersions
   , Reader Memory.DB
   , Error AppError
   , Embed IO
   ]

-- | 'Context' is the environment required to run 'CoreEffsPure' to 'IO'
data PureContext = PureContext
  { _contextLogConfig     :: KL.LogConfig
  , _contextPrometheusEnv :: Maybe P.PrometheusEnv
  , _contextDB            :: Memory.DB
  , _contextVersions      :: Memory.DBVersions
  }

makeLenses ''PureContext

makePureContext
  :: KL.InitialLogNamespace
  -> Maybe P.MetricsScrapingConfig
  -> IO PureContext
makePureContext KL.InitialLogNamespace{..} scrapingCfg = do
  metCfg <- case scrapingCfg of
        Nothing -> pure Nothing
        Just scfg -> P.emptyState >>= \es ->
          pure . Just $ P.PrometheusEnv es scfg
  logCfg <- mkLogConfig _initialLogEnvironment _initialLogProcessName
  versions <- Memory.initDBVersions
  db <- Memory.initDB
  pure $ PureContext
    { _contextLogConfig = logCfg
    , _contextPrometheusEnv = metCfg
    , _contextVersions = versions
    , _contextDB = db
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

-- | The standard interpeter for 'CoreEffsPure'.
runCoreEffsPure
  :: PureContext
  -> forall a. Sem CoreEffsPure a -> IO (Either AppError a)
runCoreEffsPure PureContext{..} =
  runM .
    runError .
    runReader _contextDB .
    runReader _contextVersions .
    runReader _contextPrometheusEnv .
    runReader _contextLogConfig
