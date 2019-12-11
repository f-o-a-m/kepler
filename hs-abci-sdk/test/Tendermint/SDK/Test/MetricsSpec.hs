{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.MetricsSpec where

import           Control.Concurrent.MVar                       (newMVar,
                                                                readMVar)
import           Data.Map.Strict                               ((!))
import qualified Data.Map.Strict                               as Map
import           Polysemy
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import qualified System.Metrics.Prometheus.Metric              as Metric
import qualified System.Metrics.Prometheus.Metric.Counter      as Counter
import qualified System.Metrics.Prometheus.Metric.Histogram    as Histogram
import qualified System.Metrics.Prometheus.Registry            as RSample
import           Tendermint.SDK.BaseApp.Metrics
import           Tendermint.SDK.BaseApp.Metrics.Prometheus
import           Test.Hspec

data Fox m a where
  Shine :: Fox m ()

makeSem ''Fox

evalFox :: Sem (Fox ': r) a -> Sem r a
evalFox = interpret $ \case
  Shine -> pure ()

eval
  :: MetricsState
  -> Sem [Fox, Metrics, Embed IO] a
  -> IO a
eval s = runM . evalMetrics s . evalFox

spec :: Spec
spec = describe "Metrics tests" $ do
  let countName = "testCount"
      c = countToIdentifier countName
      cid = metricIdStorable c
      cMetricId = mkPrometheusMetricId c
  it "Can make a new count and increment it" $ do
    state@MetricsState{..} <- emptyState
    -- new count = 0
    _ <- eval state $ incCount countName
    newCtrIndex <- readMVar metricsCounters
    newCtrValue <- Counter.sample $ newCtrIndex ! cid
    Counter.unCounterSample newCtrValue `shouldBe` 0
    -- register should contain new counter metric
    newRegistrySample <- Registry.sample metricsRegistry
    let registryMap = RSample.unRegistrySample newRegistrySample
        (Metric.CounterMetricSample registryCtrSample) = registryMap ! cMetricId
    Counter.unCounterSample registryCtrSample `shouldBe` 0
    -- increment
    _ <- eval state $ incCount countName
    incCtrIndex <- readMVar metricsCounters
    incCtrValue <- Counter.sample $ incCtrIndex ! cid
    Counter.unCounterSample incCtrValue `shouldBe` 1

  let histName = "testHistogram"
      h = histogramToIdentifier histName
      hid = metricIdStorable h
      hMetricId = mkPrometheusMetricId h
  it "Can make a new histogram and observe it" $ do
    state@MetricsState{..} <- emptyState
    -- new histogram
    _ <- eval state $ observeHistogram histName 0.0
    newHistIndex <- readMVar metricsHistograms
    newHistValue <- Histogram.sample $ newHistIndex ! hid
    Histogram.histSum newHistValue `shouldBe` 0.0
    Histogram.histCount newHistValue `shouldBe` 0
    -- register should contain new histogram metric
    newRegistrySample <- Registry.sample metricsRegistry
    let registryMap = RSample.unRegistrySample newRegistrySample
        (Metric.HistogramMetricSample registryHistSample) = registryMap ! hMetricId
    Histogram.histSum registryHistSample `shouldBe` 0.0
    Histogram.histCount registryHistSample `shouldBe` 0
    -- observe
    _ <- eval state $ observeHistogram histName 42.0
    obsHistIndex <- readMVar metricsHistograms
    obsHistValue <- Histogram.sample $ obsHistIndex ! hid
    Histogram.histSum obsHistValue `shouldBe` 42.0
    Histogram.histCount obsHistValue `shouldBe` 1

  let buckets = [0.0001, 0.001, 0.01, 0.1, 0.25, 0.5, 0.75, 1, 10]
      buckettedHistName = HistogramName "buckets" [] buckets
      buckettedH = histogramToIdentifier buckettedHistName
      buckettedHMetricId = mkPrometheusMetricId buckettedH
  it "Can measure action response times" $ do
    state@MetricsState{..} <- emptyState
    -- create a new hist
    _ <- eval state $ observeHistogram buckettedHistName 0.0
    -- time an action
    (_, time) <- eval state $ withTimer buckettedHistName shine
    time `shouldSatisfy` (> 0)
    -- check registry hist buckets
    newRegistrySample <- Registry.sample metricsRegistry
    let registryMap = RSample.unRegistrySample newRegistrySample
        (Metric.HistogramMetricSample registryHistSample) = registryMap ! buckettedHMetricId
        histBuckets = Histogram.histBuckets registryHistSample
    Map.elems histBuckets `shouldSatisfy` atLeastOneUpdated
      where atLeastOneUpdated = foldr (\b acc -> acc || (b /= 0.0)) False
