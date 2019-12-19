{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Logger
  ( Logger(..)
  , Tendermint.SDK.BaseApp.Logger.log
  , addContext
  , LogSelect(..)
  , Select(..)
  , Severity(..)
  , Verbosity(..)
  ) where

import           Data.Aeson (ToJSON)
import           Data.Text  (Text)
import           Polysemy   (makeSem)

data Severity = Debug | Info | Warning | Error | Exception deriving (Eq, Ord)
data LogSelect = All | Some [Text]
data Verbosity = V0 | V1 | V2 | V3

class Select a where
  select :: Verbosity -> a -> LogSelect

-- | Effect allowing for console logging.
data Logger m a where
  Log :: Severity -> Text -> Logger m ()
  AddContext :: (Select x, ToJSON x) => x -> m a -> Logger m a

makeSem ''Logger
