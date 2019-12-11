{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp.CoreEff
  ( CoreEffs
  , Context(..)
  , makeContext
  , runCoreEffs
  ) where

import           Control.Lens                               (over, view)
import qualified Katip                                      as K
import           Polysemy                                   (Embed, Members,
                                                             Sem, runM)
import           Polysemy.Reader                            (Reader, asks,
                                                             local, runReader)
import           Tendermint.SDK.BaseApp.Events              (EventBuffer,
                                                             newEventBuffer)
import qualified Tendermint.SDK.BaseApp.Logger.Katip        as KL
import           Tendermint.SDK.BaseApp.Store               (MergeScopes,
                                                             ResolveScope (..))
import qualified Tendermint.SDK.BaseApp.Store.AuthTreeStore as AT

-- | CoreEffs is one level below BaseAppEffs, and provides one possible
-- | interpretation for its effects to IO.
type CoreEffs =
  '[ Reader EventBuffer
   , MergeScopes
   , Reader KL.LogConfig
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
  { contextLogConfig   :: KL.LogConfig
  , contextEventBuffer :: EventBuffer
  , contextAuthTree    :: AT.AuthTreeState
  }

makeContext :: KL.LogConfig -> IO Context
makeContext logCfg = do
  authTreeState <- AT.initAuthTreeState
  eb <- newEventBuffer
  pure $ Context
    { contextLogConfig = logCfg
    , contextEventBuffer = eb
    , contextAuthTree = authTreeState
    }

instance (Members CoreEffs r, AT.AuthTreeGetter s) => ResolveScope s r where
  resolveScope = AT.evalTagged

-- | The standard interpeter for 'CoreEffs'.
runCoreEffs
  :: Context
  -> forall a. Sem CoreEffs a -> IO a
runCoreEffs Context{..} =
  runM .
    runReader contextAuthTree .
    runReader contextLogConfig .
    AT.evalMergeScopes .
    runReader contextEventBuffer
