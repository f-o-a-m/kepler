{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp where

import           Control.Exception            (throwIO)
import           Control.Lens                 (over, view)
import qualified Katip                        as K
import           Polysemy                     (Embed, Member, Members, Sem,
                                               runM)
import           Polysemy.Error               (Error, runError)
import           Polysemy.Output              (Output)
import           Polysemy.Reader              (Reader, asks, local, runReader)
import           Polysemy.Resource            (Resource, resourceToIO)
import qualified Tendermint.SDK.AuthTreeStore as AT
import           Tendermint.SDK.Errors        (AppError)
import           Tendermint.SDK.Events        (Event, EventBuffer,
                                               evalWithBuffer, newEventBuffer)
import           Tendermint.SDK.Logger        (Logger)
import qualified Tendermint.SDK.Logger.Katip  as KL
import           Tendermint.SDK.Store         (RawStore)

-- @TODO: change to HasBassAppEff
type HasBaseAppEff r =
  ( Member Logger r
  , Member (Error AppError) r
  , Member RawStore r
  , Member (Output Event) r
  , Member Resource r
  )

data Context = Context
  { contextLogConfig   :: KL.LogConfig
  , contextEventBuffer :: EventBuffer
  , contextAuthTree    :: AT.AuthTree
  }

type CoreEffR =
  '[ Reader EventBuffer
   , Reader KL.LogConfig
   , Embed IO
   ]

type BaseAppEffR =
  [ Output Event
  , RawStore
  , Logger
  , Resource
  , Error AppError
  ]

type BaseApp = BaseAppEffR :& CoreEffR

instance (Members CoreEffR r) => K.Katip (Sem r)  where
  getLogEnv = asks $ view KL.logEnv
  localLogEnv f m = local (over KL.logEnv f) m

instance (Members CoreEffR r) => K.KatipContext (Sem r) where
  getKatipContext = asks $ view KL.logContext
  localKatipContext f m = local (over KL.logContext f) m
  getKatipNamespace = asks $ view KL.logNamespace
  localKatipNamespace f m = local (over KL.logNamespace f) m


makeContext :: KL.LogConfig -> IO Context
makeContext logCfg = do
  authTree <- AT.initAuthTree
  eb <- newEventBuffer
  pure $ Context
    { contextLogConfig = logCfg
    , contextEventBuffer = eb
    , contextAuthTree = authTree
    }

type family (as :: [a]) :& (bs :: [a]) :: [a] where
  '[] :& bs = bs
  (a ': as) :& bs = a ': (as :& bs)

infixr 5 :&

-- NOTE: Look into using 'reinterpretH'
compileToCoreEff
  :: Context
  -> Sem BaseApp a
  -> Sem CoreEffR (Either AppError a)
compileToCoreEff Context{contextAuthTree} =
  runError .
    resourceToIO .
    KL.evalKatip .
    AT.eval contextAuthTree .
    evalWithBuffer

eval
  :: Context
  -> Sem BaseApp a
  -> IO a
eval ctx@Context{..} action = do
  eRes <- runM .
    runReader contextLogConfig .
    runReader contextEventBuffer .
    compileToCoreEff ctx $ action
  either throwIO return eRes
