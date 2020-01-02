{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Logger
  ( Logger(..)
  , Tendermint.SDK.BaseApp.Logger.log
  , Severity(..)
  ) where

import           Data.Text (Text)
import           Polysemy  (makeSem)

data Severity = Debug | Info | Warning | Error | Exception deriving (Eq, Ord)

-- | Effect allowing for console logging.
data Logger m a where
  Log :: Severity -> Text -> Logger m ()
  -- AddContext :: Object -> m a -> Logger m a

makeSem ''Logger
