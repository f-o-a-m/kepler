{-# LANGUAGE TemplateHaskell #-}
module Tendermint.SDK.Logger.Katip where

import           Control.Lens.TH         (makeLenses)
import           Data.String             (fromString)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import qualified Katip                   as K
import           Polysemy                (Sem, interpret)
import           Tendermint.SDK.Logger   (Logger (..), Severity (..))

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

evalKatip
  :: forall r a.
     K.Katip (Sem r)
  => K.KatipContext (Sem r)
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
