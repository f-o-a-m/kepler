{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SimpleStorage.Logging
  ( LogConfig(..)
  , HasLogConfig(..)
  , mkLogConfig
  ) where

import           Control.Lens           (Lens', over, view)
import           Control.Lens.TH        (makeLenses)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Text              (Text)
import qualified Katip                  as K

data LogConfig = LogConfig
  { _logNamespace :: K.Namespace
  , _logContext   :: K.LogContexts
  , _logEnv       :: K.LogEnv
  }
makeLenses ''LogConfig

class HasLogConfig config where
  logConfig :: Lens' config LogConfig

instance (MonadIO m, MonadReader config m, HasLogConfig config) => K.Katip m where
  getLogEnv = view (logConfig . logEnv)
  localLogEnv f m = local (over (logConfig . logEnv) f) m

instance (MonadIO m, MonadReader config m, HasLogConfig config) => K.KatipContext m where
  getKatipContext = view (logConfig . logContext)
  localKatipContext f m = local (over (logConfig . logContext) f) m
  getKatipNamespace = view (logConfig . logNamespace)
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