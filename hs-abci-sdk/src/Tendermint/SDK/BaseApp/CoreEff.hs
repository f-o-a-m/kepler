{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp.CoreEff
  ( CoreEffs
  , Context(..)
  , contextLogConfig
  , contextPrometheusEnv
  , contextAuthTree
  , makeContext
  , runCoreEffs
  ) where

import           Control.Lens                               (makeLenses, over,
                                                             view)
import           Data.Text                                  (Text)
import qualified Katip                                      as K
import           Polysemy                                   (Embed, Members,
                                                             Sem, runM)
import           Polysemy.Reader                            (Reader, asks,
                                                             local, runReader)
import qualified Tendermint.SDK.BaseApp.Logger.Katip        as KL
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus  as P
import           Tendermint.SDK.BaseApp.Store               (MergeScopes,
                                                             ResolveScope (..))
import qualified Tendermint.SDK.BaseApp.Store.AuthTreeStore as AT

-- | CoreEffs is one level below BaseAppEffs, and provides one possible
-- | interpretation for its effects to IO.
type CoreEffs =
  '[ MergeScopes
   , Reader KL.LogConfig
   , Reader (Maybe P.PrometheusEnv)
   , Reader AT.AuthTreeState
   , Embed IO
   ]

instance (Members CoreEffs r) => K.Katip (Sem r)  where
  getLogEnv = asks $ view KL.logEnv
  localLogEnv f m = local (over KL.logEnv f) m

instance (Members CoreEffs r) => K.KatipContext (Sem r) where
  getKatipContext = asks $ view KL.logContext
  localKatipContext f m = local (over KL.logContext f) m
  getKatipNamespace = asks $ view KL.logNamespace
  localKatipNamespace f m = local (over KL.logNamespace f) m

-- | 'Context' is the environment required to run 'CoreEffs' to 'IO'
data Context = Context
  { _contextLogConfig     :: KL.LogConfig
  , _contextPrometheusEnv :: Maybe P.PrometheusEnv
  , _contextAuthTree      :: AT.AuthTreeState
  }

makeLenses ''Context

makeContext
  :: KL.InitialLogNamespace
  -> Maybe P.MetricsScrapingConfig
  -> IO Context
makeContext KL.InitialLogNamespace{..} scrapingCfg = do
  metCfg <- case scrapingCfg of
        Nothing -> pure Nothing
        Just scfg -> P.emptyState >>= \es ->
          pure . Just $ P.PrometheusEnv es scfg
  authTreeState <- AT.initAuthTreeState
  logCfg <- mkLogConfig _initialLogEnvironment _initialLogProcessName
  pure $ Context
    { _contextLogConfig = logCfg
    , _contextPrometheusEnv = metCfg
    , _contextAuthTree = authTreeState
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

instance (Members CoreEffs r, AT.AuthTreeGetter s) => ResolveScope s r where
  resolveScope = AT.evalTagged

-- | The standard interpeter for 'CoreEffs'.
runCoreEffs
  :: Context
  -> forall a. Sem CoreEffs a -> IO a
runCoreEffs Context{..} =
  runM .
    runReader _contextAuthTree .
    runReader _contextPrometheusEnv .
    runReader _contextLogConfig .
    AT.evalMergeScopes
