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
import qualified Data.Aeson                    as A
import           Data.String                   (fromString)
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import qualified Katip                         as K
import           Polysemy                      (Sem, interpretH, pureT, raise,
                                                runT)
import           Tendermint.SDK.BaseApp.Logger

newtype Object a = Object a

instance Select a => Select (Object a) where
  select v (Object x) = select v x

instance A.ToJSON a => K.ToObject (Object a) where
  toObject (Object a) = case A.toJSON a of
      A.Object o -> o
      _          -> mempty

instance (A.ToJSON a, Select a) => K.LogItem (Object a) where
  payloadKeys = interpretFromSelect
    where
      interpretFromSelect kVerbosity obj =
        let selectRes = select (kVerbToVerb kVerbosity) obj
        in case selectRes of
          All     -> K.AllKeys
          Some ts -> K.SomeKeys ts
      kVerbToVerb K.V0 = V0
      kVerbToVerb K.V1 = V1
      kVerbToVerb K.V2 = V2
      kVerbToVerb K.V3 = V3

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
  interpretH (\case
    Log severity msg -> do
      raise $ K.localKatipNamespace (<> "application") $
        K.logFM (coerceSeverity severity) (fromString . cs $ msg)
      pureT ()
    AddContext obj action -> do
      a <- runT action
      raise $ K.katipAddContext (Object obj) (evalKatip a)
    )
    where
      coerceSeverity :: Severity -> K.Severity
      coerceSeverity = \case
        Debug -> K.DebugS
        Info -> K.InfoS
        Warning -> K.WarningS
        Error -> K.ErrorS
        Exception -> K.CriticalS
