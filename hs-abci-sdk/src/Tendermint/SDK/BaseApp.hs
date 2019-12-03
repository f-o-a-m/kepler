{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp where

import           Control.Exception            (throwIO)
import           Control.Lens                 (over, view)
import qualified Katip                        as K
import           Polysemy                     (Embed, Members, Sem, runM)
import           Polysemy.Error               (Error, runError)
import           Polysemy.Output              (Output)
import           Polysemy.Reader              (Reader, asks, local, runReader)
import           Polysemy.Resource            (Resource, resourceToIO)
import           Polysemy.Tagged              (Tagged)
import qualified Tendermint.SDK.AuthTreeStore as AT
import           Tendermint.SDK.Errors        (AppError)
import           Tendermint.SDK.Events        (Event, EventBuffer,
                                               evalWithBuffer, newEventBuffer)
import           Tendermint.SDK.Logger        (Logger)
import qualified Tendermint.SDK.Logger.Katip  as KL
import           Tendermint.SDK.Store         (ConnectionScope (..), RawStore)


-- | Concrete row of effects for the BaseApp
type BaseAppEffs =
  [ Output Event
  , Tagged 'Query RawStore
  , Tagged 'Mempool RawStore
  , Tagged 'Consensus RawStore
  , Logger
  , Resource
  , Error AppError
  ]

-- | CoreEff is one level below BaseApp, it as a seperation of BaseApp from
-- | its interpretation.
type CoreEffs =
  '[ Reader EventBuffer
   , Reader KL.LogConfig
   , Reader AT.AuthTreeState
   , Embed IO
   ]

-- | This type family gives a nice syntax for combining multiple lists of effects.
type family (as :: [a]) :& (bs :: [a]) :: [a] where
  '[] :& bs = bs
  (a ': as) :& bs = a ': (as :& bs)

infixr 5 :&

-- TODO: it would be really nice to not have this CoreEff here, but to just
-- interpret into it as an intermediate step in the total evaluation of BaseApp.
-- Polysemy probably has a way to do this already.
type BaseApp = BaseAppEffs :& CoreEffs

instance (Members CoreEffs r) => K.Katip (Sem r)  where
  getLogEnv = asks $ view KL.logEnv
  localLogEnv f m = local (over KL.logEnv f) m

instance (Members CoreEffs r) => K.KatipContext (Sem r) where
  getKatipContext = asks $ view KL.logContext
  localKatipContext f m = local (over KL.logContext f) m
  getKatipNamespace = asks $ view KL.logNamespace
  localKatipNamespace f m = local (over KL.logNamespace f) m

-- | 'Context' is the environment required to run 'CoreEff' to 'IO'
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

-- | An intermediary interpeter, bringing 'BaseApp' down to 'CoreEff'.
compileToCoreEff
  :: Sem BaseApp a
  -> Sem CoreEffs (Either AppError a)
compileToCoreEff =
  runError .
    resourceToIO .
    KL.evalKatip .
    AT.eval .
    evalWithBuffer

-- | The standard interpeter for 'BaseApp'.
eval
  :: Context
  -> Sem BaseApp a
  -> IO a
eval Context{..} action = do
  eRes <- runM .
    runReader contextAuthTree .
    runReader contextLogConfig .
    runReader contextEventBuffer .
    compileToCoreEff $ action
  either throwIO return eRes
