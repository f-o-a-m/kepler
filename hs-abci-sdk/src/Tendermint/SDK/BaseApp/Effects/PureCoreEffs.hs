{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.BaseApp.Effects.PureCoreEffs
  ( PureCoreEffs
  , PureContext(..)
  , pureContextLogConfig
  , pureContextVersions
  , pureContextDB
  , makePureContext
  , runPureCoreEffs
  ) where

import           Control.Lens                             (makeLenses)
import           Data.Text                                (Text)
import qualified Katip                                    as K
import           Polysemy                                 (Embed, Sem, runM)
import           Polysemy.Error                           (Error, runError)
import           Polysemy.Reader                          (Reader, runReader)
import           Tendermint.SDK.BaseApp.Errors            (AppError)
import qualified Tendermint.SDK.BaseApp.Logger.Katip      as KL
import qualified Tendermint.SDK.BaseApp.Store.MemoryStore as Memory

-- | CoreEffs is one level below BaseAppEffs, and provides one possible
-- | interpretation for its effects to IO.
type PureCoreEffs =
  '[ Reader KL.LogConfig
   , Reader Memory.DBVersions
   , Reader Memory.DB
   , Error AppError
   , Embed IO
   ]

-- | 'Context' is the environment required to run 'CoreEffsPure' to 'IO'
data PureContext = PureContext
  { _pureContextLogConfig :: KL.LogConfig
  , _pureContextDB        :: Memory.DB
  , _pureContextVersions  :: Memory.DBVersions
  }

makeLenses ''PureContext

makePureContext
  :: KL.InitialLogNamespace
  -> IO PureContext
makePureContext KL.InitialLogNamespace{..}  = do
  logCfg <- mkLogConfig _initialLogEnvironment _initialLogProcessName
  versions <- Memory.initDBVersions
  db <- Memory.initDB
  pure $ PureContext
    { _pureContextLogConfig = logCfg
    , _pureContextVersions = versions
    , _pureContextDB = db
    }
    where
      mkLogConfig :: Text -> Text -> IO KL.LogConfig
      mkLogConfig env pName = do
        let mkLogEnv = K.initLogEnv (K.Namespace [pName]) (K.Environment env)
        le <- mkLogEnv
        return $ KL.LogConfig
          { _logNamespace = mempty
          , _logContext = mempty
          , _logEnv = le
          }

-- | The standard interpeter for 'PureCoreEffs'.
runPureCoreEffs
  :: PureContext
  -> forall a. Sem PureCoreEffs a -> IO (Either AppError a)
runPureCoreEffs PureContext{..} =
  runM .
    runError .
    runReader _pureContextDB .
    runReader _pureContextVersions .
    runReader _pureContextLogConfig
