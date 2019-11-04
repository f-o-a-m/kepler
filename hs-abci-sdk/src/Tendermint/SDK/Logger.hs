{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.Logger where

import           Control.Lens    (over)
import           Control.Lens.TH (makeLenses)
import           Data.String     (fromString)
import           Data.Text       (Text)
import qualified Katip           as K
import           Polysemy        (Embed, Member, Sem, interpret, makeSem)
import           Polysemy.Reader (Reader, asks, local)

data Logger m a where
  Log :: K.Severity -> String -> Logger m ()

makeSem ''Logger

data LogConfig = LogConfig
  { _logNamespace :: K.Namespace
  , _logContext   :: K.LogContexts
  , _logEnv       :: K.LogEnv
  }
makeLenses ''LogConfig

mkLogConfig :: Text -> Text -> IO LogConfig
mkLogConfig environment processName = do
    le <- mkLogEnv
    return $ LogConfig
      { _logNamespace = mempty
      , _logContext = mempty
      , _logEnv = le
      }
  where
    mkLogEnv = K.initLogEnv (K.Namespace [processName]) (K.Environment environment)

instance (Member (Embed IO) r, Member (Reader LogConfig) r) => K.Katip (Sem r) where
  getLogEnv = asks _logEnv
  localLogEnv f m = local (over logEnv f) m

instance (Member (Embed IO) r, Member (Reader LogConfig) r) => K.KatipContext (Sem r) where
  getKatipContext = asks _logContext
  localKatipContext f m = local (over logContext f) m
  getKatipNamespace = asks _logNamespace
  localKatipNamespace f m = local (over logNamespace f) m

evalKatip
  :: forall r a.
     K.Katip (Sem r)
  => K.KatipContext (Sem r)
  => Sem (Logger ': r) a
  -> Sem r a
evalKatip = do
  interpret (\case
    Log severity msg -> K.logFM severity (fromString msg)
    )
