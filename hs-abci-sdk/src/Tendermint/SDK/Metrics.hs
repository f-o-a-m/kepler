{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Metrics where

import           Data.Text (Text)
import           Polysemy

data CountName = CountName
  { countName   :: Text
  , countLabels :: [(Text, Text)]
  } deriving (Eq, Ord)

data HistogramName = HistogramName
  { histogramName    :: Text
  , histogramLabels  :: [(Text, Text)]
  , histogramBuckets :: [Double]
  } deriving (Eq, Ord)


data Metrics m a where
  -- | Increments the count of a specific message
  IncCount :: CountName -> Metrics m ()
  -- | Update a histogram
  ObserveHistogram :: HistogramName -> Double -> Metrics m ()
  -- | Times an action
  WithTimer :: HistogramName -> m a -> Metrics m (a, Double)

makeSem ''Metrics
