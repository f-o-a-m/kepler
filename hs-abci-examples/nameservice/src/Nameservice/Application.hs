{-# LANGUAGE UndecidableInstances #-}

module Nameservice.Application
  ( AppError(..)
  , AppConfig(..)
  , makeAppConfig
  , Handler
  , compileToBaseApp
  , runHandler
  ) where

import           Control.Exception           (Exception)
import           Polysemy                    (Sem)
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

compileToBaseApp :: Sem EffR a -> Sem BaseApp.BaseApp a
compileToBaseApp = A.eval . T.eval . N.eval

-- NOTE: this should probably go in the library
runHandler
  :: AppConfig
  -> Handler a
  -> IO a
runHandler AppConfig{baseAppContext} =
  BaseApp.eval baseAppContext . compileToBaseApp
