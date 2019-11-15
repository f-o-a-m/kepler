{-# LANGUAGE UndecidableInstances #-}

module Nameservice.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , runHandler
  ) where

import           Control.Exception           (Exception)
import           Control.Monad.Catch         (throwM)
import           Polysemy                    (Sem)
import           Polysemy.Error              (Error, runError)
import qualified Tendermint.SDK.BaseApp      as BaseApp
import qualified Tendermint.SDK.Logger.Katip as KL
import qualified Tendermint.SDK.Auth as A
import qualified Nameservice.Modules.Nameservice as N
import qualified Nameservice.Modules.Token as T

data AppConfig = AppConfig
  { baseAppContext :: BaseApp.Context
  }

makeAppConfig :: KL.LogConfig -> IO AppConfig
makeAppConfig logCfg = do
  c <- BaseApp.makeContext logCfg
  pure $ AppConfig { baseAppContext = c
                   }

--------------------------------------------------------------------------------

data AppError = AppError String deriving (Show)

instance Exception AppError

type family (as :: [a]) :& (bs :: [a]) :: [a] where 
  '[] :& bs = bs
  (a ': as) :& bs = a ': (as :& bs)

type EffR =
  N.NameserviceEffR :& (T.TokenEffR :& (A.AuthEffR :& BaseApp.BaseApp))

type Handler = Sem EffR

-- NOTE: this should probably go in the library
runHandler
  :: AppConfig
  -> Handler a
  -> IO a
runHandler AppConfig{baseAppContext} m = do
  BaseApp.eval baseAppContext .
    A.eval .
    T.eval .
    N.eval $ m
