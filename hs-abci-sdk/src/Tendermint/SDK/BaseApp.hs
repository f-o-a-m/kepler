{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp where

import           Control.Exception            (throwIO)
import           Control.Lens                 (over, view)
import           Control.Monad.IO.Class       (liftIO)
import qualified Katip                        as K
import           Polysemy                     (EffectRow, Embed, Members, Sem,
                                               rewrite, runM)
import           Polysemy.Error               (Error, runError)
import           Polysemy.Output              (Output)
import           Polysemy.Reader              (Reader, asks, local, runReader)
import           Polysemy.Resource            (Resource, resourceToIO)
import           Polysemy.Tagged              (Tagged (..))
import qualified Tendermint.SDK.AuthTreeStore as AT
import           Tendermint.SDK.Errors        (AppError)
import           Tendermint.SDK.Events        (Event, EventBuffer,
                                               evalWithBuffer, newEventBuffer)
import           Tendermint.SDK.Logger        (Logger)
import qualified Tendermint.SDK.Logger.Katip  as KL
import           Tendermint.SDK.Store         (ConnectionScope (..),
                                               MergeScopes, RawStore)

-- | Concrete row of effects for the BaseApp
type BaseAppEffs =
  [ RawStore
  , Output Event
  , Logger
  , Resource
  , Error AppError
  ]

-- | CoreEff is one level below BaseApp, it as a seperation of BaseApp from
-- | its interpretation.
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

type family ApplyScope (s :: ConnectionScope) (es :: EffectRow) :: EffectRow where
--  ApplyScope 'UnScoped (RawStore ': as) = as
--  ApplyScope 'UnScoped (a ': as) = ApplyScope 'Unscoped as
  ApplyScope s (RawStore ': as) = Tagged s RawStore ': as
  ApplyScope s (a ': as) = a ': ApplyScope s as

type ScopedBaseApp s = ApplyScope s BaseApp

applyScope
  :: forall s.
     forall a. Sem BaseApp a -> Sem (ScopedBaseApp s) a
applyScope = rewrite (Tagged @s)

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

--data Handler a where
--  ScopedHandler :: AT.AuthTreeGetter s => Sem (ScopedBaseApp s) a -> ScopedHandler a
--  UnscopedHandler :: Sem BaseApp a -> ScopedHandler a

-- | The standard interpeter for 'CoreEffs'.
eval
  :: Context
  -> forall a. Sem CoreEffs a -> IO a
eval Context{..} =
  runM .
    runReader contextAuthTree .
    runReader contextLogConfig .
    AT.evalMergeScopes .
    runReader contextEventBuffer
