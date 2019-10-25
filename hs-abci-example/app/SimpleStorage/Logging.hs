{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SimpleStorage.Logging
  ( LogConfig(..)
  , HasLogConfig(..)
  , mkLogConfig
  ) where

import           Control.Lens    (Lens', over, view)
import           Control.Lens.TH (makeLenses)
import           Data.Text       (Text)
import qualified Katip           as K
import           Polysemy
import           Polysemy.Reader

data LogConfig = LogConfig
  { _logNamespace :: K.Namespace
  , _logContext   :: K.LogContexts
  , _logEnv       :: K.LogEnv
  }
makeLenses ''LogConfig

class HasLogConfig config where
  logConfig :: Lens' config LogConfig

instance (Member (Embed IO) r, HasLogConfig config) => K.Katip (Sem (Reader config ': r)) where
  getLogEnv = asks (_logEnv . view logConfig)
  localLogEnv f m = local (over (logConfig . logEnv) f) m

instance (Member (Embed IO) r, HasLogConfig config) => K.KatipContext (Sem (Reader config ': r)) where
  getKatipContext = asks (_logContext . view logConfig )
  localKatipContext f m = local (over (logConfig . logContext) f) m
  getKatipNamespace = asks (_logNamespace . view logConfig)
  localKatipNamespace f m = local (over (logConfig . logNamespace) f) m

mkLogConfig :: Text -> IO LogConfig
mkLogConfig processName = do
    le <- mkLogEnv
    return $ LogConfig
      { _logNamespace = mempty
      , _logContext = mempty
      , _logEnv = le
      }
  where
    mkLogEnv = K.initLogEnv (K.Namespace [processName]) (K.Environment "development")
