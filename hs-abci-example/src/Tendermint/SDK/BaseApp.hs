{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp where

import           Control.Lens                 (makeLenses, over, view)
import qualified Katip                        as K
import           Polysemy                     (Embed, Member, Members, Sem)
import           Polysemy.Output              (Output)
import           Polysemy.Reader              (Reader, ask, asks, local)
import           Polysemy.Resource            (Resource, resourceToIO)
import           Tendermint.SDK.AuthTreeStore (AuthTreeDriver,
                                               interpretAuthTreeStore)
import           Tendermint.SDK.Events        (Event, EventBuffer,
                                               evalWithBuffer)
import           Tendermint.SDK.Logger        (Logger)
import qualified Tendermint.SDK.Logger.Katip  as KL
import           Tendermint.SDK.Store         (RawStore)

type HasBaseApp r =
  ( Member Logger r
  , Member RawStore r
  , Member (Output Event) r
  , Member Resource r
  )

data BaseAppContext logCfg = BaseAppContext
  { _baseAppLogConfig      :: logCfg
  , _baseAppEventBuffer    :: EventBuffer
  , _baseAppAuthTreeDriver :: AuthTreeDriver
  }
makeLenses ''BaseAppContext

type CoreEff =
  '[ Reader (BaseAppContext KL.LogConfig)
   , Embed IO
   ]

type BaseApp =
  (  Output Event
  ': RawStore
  ': Logger
  ': Resource
  ': CoreEff
  )

instance (Members CoreEff r) => K.Katip (Sem r)  where
  getLogEnv = asks $ view (baseAppLogConfig . KL.logEnv)
  localLogEnv f m = local (over (baseAppLogConfig . KL.logEnv) f) m

instance (Members CoreEff r) => K.KatipContext (Sem r)  where
  getKatipContext = asks $ view (baseAppLogConfig . KL.logContext)
  localKatipContext f m = local (over (baseAppLogConfig . KL.logContext) f) m
  getKatipNamespace = asks $ view (baseAppLogConfig . KL.logNamespace)
  localKatipNamespace f m = local (over (baseAppLogConfig . KL.logNamespace) f) m



-- NOTE: Do we need this step? I think so because of the logger.
-- You don't want to run against a fresh katip context every time.
evalToCoreEff
  :: Sem BaseApp a
  -> Sem CoreEff a
evalToCoreEff action = do
  BaseAppContext{..} <- ask
  resourceToIO .
    KL.evalKatip .
    interpretAuthTreeStore _baseAppAuthTreeDriver .
    evalWithBuffer _baseAppEventBuffer $ action
