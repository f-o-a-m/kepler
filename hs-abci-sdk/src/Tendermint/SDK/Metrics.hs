{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Metrics where

import           Data.Text (Text)
import           Polysemy

newtype CountName = CountName { unCountName :: Text }
  deriving (Eq, Ord)

newtype HistogramName = HistogramName { unHistrogramName :: Text }
  deriving (Eq, Ord)

data Metrics m a where
  -- | Get a counter
  -- GetCount :: CountName -> Metrics m a
  -- | Increments the count of a specific message
  IncCount :: CountName -> Metrics m ()
  -- | Get a histogram
  -- GetHistogram :: HistogramName -> Metrics m a
  -- | Update a histogram
  ObserveHistogram :: HistogramName -> Double -> Metrics m ()
  -- | Times an action
  WithTimer :: HistogramName -> m a -> Metrics m (a, Double)

makeSem ''Metrics
