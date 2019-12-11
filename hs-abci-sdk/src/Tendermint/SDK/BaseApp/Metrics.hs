{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Metrics where

import           Data.String (IsString (..))
import           Data.Text   (Text)
import           Polysemy

data CountName = CountName
  { countName   :: Text
  , countLabels :: [(Text, Text)]
  } deriving (Eq, Ord)

instance IsString CountName where
  fromString s = CountName (fromString s) mempty

data HistogramName = HistogramName
  { histogramName    :: Text
  , histogramLabels  :: [(Text, Text)]
  , histogramBuckets :: [Double]
  } deriving (Eq, Ord)

instance IsString HistogramName where
  fromString s = HistogramName (fromString s) mempty mempty

-- @NOTE: might need a NewCount/NewHistogram summand to clean up some usage quirks
data Metrics m a where
  -- | Increments the count of a specific message
  IncCount :: CountName -> Metrics m ()
  -- | Update a histogram
  ObserveHistogram :: HistogramName -> Double -> Metrics m ()
  -- | Times an action
  WithTimer :: HistogramName -> m a -> Metrics m (a, Double)

makeSem ''Metrics
