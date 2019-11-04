{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Logger
  ( Logger(..)
  , Tendermint.SDK.Logger.log
  , Severity(..)
  ) where

import           Data.Text (Text)
import           Polysemy  (makeSem)

data Severity = Debug | Info | Warning | Error | Exception deriving (Eq, Ord)

data Logger m a where
  Log :: Severity -> Text -> Logger m ()

makeSem ''Logger
