{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Logger.Katip 
  ( -- setup and config
    LogConfig(..)
  , logNamespace
  , logContext
  , logEnv
  , InitialLogNamespace(..)
  , initialLogEnvironment
  , initialLogProcessName
  -- eval
  , evalKatip
  ) where

import           Control.Lens.TH               (makeLenses)
import           Data.String                   (fromString)
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import qualified Katip                         as K
import           Polysemy                      (Sem, interpret)
import           Tendermint.SDK.BaseApp.Logger (Logger (..), Severity (..))

data LogConfig = LogConfig
  { _logNamespace :: K.Namespace
  , _logContext   :: K.LogContexts
  , _logEnv       :: K.LogEnv
  }
makeLenses ''LogConfig

data InitialLogNamespace = InitialLogNamespace
  { _initialLogEnvironment :: Text
  , _initialLogProcessName :: Text
  }

makeLenses ''InitialLogNamespace

evalKatip
  :: forall r a.
     K.KatipContext (Sem r)
  => Sem (Logger ': r) a
  -> Sem r a
evalKatip = do
  interpret (\case
    Log severity msg ->
      K.localKatipNamespace (<> "application") $
        K.logFM (coerceSeverity severity) (fromString . cs $ msg)
    )
    where
      coerceSeverity :: Severity -> K.Severity
      coerceSeverity = \case
        Debug -> K.DebugS
        Info -> K.InfoS
        Warning -> K.WarningS
        Error -> K.ErrorS
        Exception -> K.CriticalS
