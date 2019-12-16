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
  fromString s = HistogramName (fromString s) mempty defaultBuckets
    where defaultBuckets = [0.0001, 0.001, 0.01, 0.1, 0.25, 0.5, 0.75, 1, 10, 100]

data Metrics m a where
  -- | Increments the count of a specific message
  IncCount :: CountName -> Metrics m ()
  -- | Times an action and records it in a histogram
  WithTimer :: HistogramName -> m a -> Metrics m ()

makeSem ''Metrics
