{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp where

import           Control.Lens                 (over, view)
import qualified Katip                        as K
import           Polysemy                     (Embed, Member, Members, Sem,
                                               runM)
import           Polysemy.Output              (Output)
import           Polysemy.Reader              (Reader, asks, local, runReader)
import           Polysemy.Resource            (Resource, resourceToIO)
import           Tendermint.SDK.AuthTreeStore (AuthTreeDriver,
                                               initAuthTreeDriver,
                                               interpretAuthTreeStore)
import           Tendermint.SDK.Events        (Event, EventBuffer,
                                               evalWithBuffer, newEventBuffer)
import           Tendermint.SDK.Logger        (Logger)
import qualified Tendermint.SDK.Logger.Katip  as KL
import           Tendermint.SDK.Store         (RawStore)
import Polysemy.Error (Error, runError)
import Tendermint.SDK.Errors (AppError)
import Control.Exception (throwIO)

-- @TODO: change to HasBassAppEff
type HasBaseApp r =
  ( Member Logger r
  , Member (Error AppError) r
  , Member RawStore r
  , Member (Output Event) r
  , Member Resource r
  )

data Context = Context
  { contextLogConfig      :: KL.LogConfig
  , contextEventBuffer    :: EventBuffer
  , contextAuthTreeDriver :: AuthTreeDriver
  }

type CoreEff =
  '[ Reader EventBuffer
   , Reader KL.LogConfig
   , Embed IO
   ]

-- @TODO: change to BaseAppEffR
type BaseApp =
  [ Output Event
  , RawStore
  , Logger
  , Resource
  , Error AppError
  ]

instance (Members CoreEff r) => K.Katip (Sem r)  where
  getLogEnv = asks $ view KL.logEnv
  localLogEnv f m = local (over KL.logEnv f) m

instance (Members CoreEff r) => K.KatipContext (Sem r) where
  getKatipContext = asks $ view KL.logContext
  localKatipContext f m = local (over KL.logContext f) m
  getKatipNamespace = asks $ view KL.logNamespace
  localKatipNamespace f m = local (over KL.logNamespace f) m


makeContext :: KL.LogConfig -> IO Context
makeContext logCfg = do
  authTreeD <- initAuthTreeDriver
  eb <- newEventBuffer
  pure $ Context
    { contextLogConfig = logCfg
    , contextEventBuffer = eb
    , contextAuthTreeDriver = authTreeD
    }

compileToCoreEff :: Context -> Sem BaseApp a -> Sem CoreEff (Either AppError a)
compileToCoreEff Context{contextAuthTreeDriver}=
  runError .
    resourceToIO .
    KL.evalKatip .
    interpretAuthTreeStore contextAuthTreeDriver

-- NOTE: Do we need this step? I think so because of the logger.
-- You don't want to run against a fresh katip context every time.
eval
  :: Context
  -> Sem BaseApp a
  -> IO a
eval ctx@Context{..} action = do
  eRes <- runM .
    runReader contextLogConfig .
    runReader contextEventBuffer .
    evalWithBuffer .
    compileToCoreEff ctx $ action
  either throwIO return eRes

--------------------------------------------------------------------------------
