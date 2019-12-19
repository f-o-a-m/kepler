{-# LANGUAGE TemplateHaskell #-}
module Tendermint.SDK.BaseApp.Logger.Katip where

import           Control.Lens.TH               (makeLenses)
import           Data.String                   (fromString)
import           Data.String.Conversions       (cs)
import qualified Katip                         as K
import           Polysemy                      (Sem, interpret)
import           Tendermint.SDK.BaseApp.Logger (Logger (..), Severity (..))

data LogConfig = LogConfig
  { _logNamespace :: K.Namespace
  , _logContext   :: K.LogContexts
  , _logEnv       :: K.LogEnv
  }
makeLenses ''LogConfig

evalKatip
  :: forall r a.
     K.KatipContext (Sem r)
  => Sem (Logger ': r) a
  -> Sem r a
evalKatip = do
  interpret (\case
    Log severity msg -> K.logFM (coerceSeverity severity) (fromString . cs $ msg)
    )
    where
      coerceSeverity :: Severity -> K.Severity
      coerceSeverity = \case
        Debug -> K.DebugS
        Info -> K.InfoS
        Warning -> K.WarningS
        Error -> K.ErrorS
        Exception -> K.CriticalS
