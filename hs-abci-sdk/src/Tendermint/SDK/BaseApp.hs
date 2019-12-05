{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp where

import           Control.Exception                  (throwIO)
import           Control.Lens                       (over, view)
import           Control.Monad.IO.Class             (liftIO)
import qualified Katip                              as K
import           Polysemy                           (Embed, Members, Sem, runM)
import           Polysemy.Error                     (Error, runError)
import           Polysemy.Output                    (Output)
import           Polysemy.Reader                    (Reader, asks, local,
                                                     runReader)
import           Polysemy.Resource                  (Resource, resourceToIO)
import           Tendermint.SDK.Errors              (AppError)
import           Tendermint.SDK.Events              (Event, EventBuffer,
                                                     evalWithBuffer,
                                                     newEventBuffer)
import           Tendermint.SDK.Logger              (Logger)
import qualified Tendermint.SDK.Logger.Katip        as KL
import           Tendermint.SDK.Store               (ApplyScope,
                                                     ConnectionScope,
                                                     MergeScopes, RawStore)
import qualified Tendermint.SDK.Store.AuthTreeStore as AT

-- | Concrete row of effects for the BaseApp. Note that because there does
-- | not exist an interpreter for an untagged 'RawStore', you must scope
-- | these effects before they can be interpreted.
type BaseAppEffs =
  [ RawStore
  , Output Event
  , Logger
  , Resource
  , Error AppError
  ]

-- | CoreEffs is one level below BaseAppEffs, and provides one possible
-- | interpretation for its effects to IO.
type CoreEffs =
  '[ Reader EventBuffer
   , MergeScopes
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

type ScopedBaseApp (s :: ConnectionScope) = ApplyScope s BaseApp

instance (Members CoreEffs r) => K.Katip (Sem r)  where
  getLogEnv = asks $ view KL.logEnv
  localLogEnv f m = local (over KL.logEnv f) m

instance (Members CoreEffs r) => K.KatipContext (Sem r) where
  getKatipContext = asks $ view KL.logContext
  localKatipContext f m = local (over KL.logContext f) m
  getKatipNamespace = asks $ view KL.logNamespace
  localKatipNamespace f m = local (over KL.logNamespace f) m

-- | An intermediary interpeter, bringing 'BaseApp' down to 'CoreEff'.
compileToCoreEff
  :: forall s.
     AT.AuthTreeGetter s
  => forall a. Sem (ScopedBaseApp s) a -> Sem CoreEffs a
compileToCoreEff action = do
  eRes <- runError .
    resourceToIO .
    KL.evalKatip .
    evalWithBuffer .
    AT.evalTagged $ action
  either (liftIO . throwIO) return eRes

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

-- | The standard interpeter for 'CoreEffs'.
evalCoreEffs
  :: Context
  -> forall a. Sem CoreEffs a -> IO a
evalCoreEffs Context{..} =
  runM .
    runReader contextAuthTree .
    runReader contextLogConfig .
    AT.evalMergeScopes .
    runReader contextEventBuffer
