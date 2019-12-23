{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Logger
  ( Logger(..)
  , Tendermint.SDK.BaseApp.Logger.log
  , addContext
  , ContextEvent(..)
  , LogSelect(..)
  , Select(..)
  , Severity(..)
  , Verbosity(..)
  ) where

import           Data.Aeson                    (ToJSON (..), object, (.=))
import           Data.Proxy
import           Data.Text                     (Text)
import           Polysemy                      (makeSem)
import           Tendermint.SDK.BaseApp.Events (ToEvent (..))

data Severity = Debug | Info | Warning | Error | Exception deriving (Eq, Ord)
data LogSelect = All | Some [Text]
data Verbosity = V0 | V1 | V2 | V3

-- | Class for selecting object keys for contextual logging
class Select a where
  select :: Verbosity -> a -> LogSelect
  default select :: Verbosity -> a -> LogSelect
  select _ _ = All

-- | Special event wrapper to add contextual event_type info
newtype ContextEvent t = ContextEvent t
instance (ToJSON a, ToEvent a) => ToJSON (ContextEvent a) where
  toJSON (ContextEvent a) =
    object [ "event_type" .= makeEventType (Proxy :: Proxy a)
           , "event" .= toJSON a
           ]
instance Select a => Select (ContextEvent a) where
  select v (ContextEvent a) = select v a

-- | Effect allowing for console logging.
data Logger m a where
  Log :: Severity -> Text -> Logger m ()
  AddContext :: (Select x, ToJSON x) => x -> m a -> Logger m a

makeSem ''Logger
