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
import qualified System.Metrics.Prometheus.MetricId            as MetricId
import qualified System.Metrics.Prometheus.Registry            as RSample
import           Tendermint.SDK.Metrics
import           Tendermint.SDK.Metrics.Metrics
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

emptyState :: IO MetricsState
emptyState = do
  counters <- newMVar Map.empty
  histos <- newMVar Map.empty
  registry <- Registry.new
  return $ MetricsState registry counters histos

spec :: Spec
spec = describe "Metrics tests" $ do
  let countName = CountName "blip" []
      c = countToIdentifier countName
      cName = fixMetricName $ metricIdName c
      cLabels = metricIdLabels c
      cid = metricIdStorable c
      cMetricIdName = MetricId.Name cName
      cMetricId = MetricId.MetricId cMetricIdName cLabels
  it "Can make a new count and increment it" $ do
    state <- emptyState
    -- new count = 0
    _ <- eval state $ incCount countName
    let counters = metricsCounters state
    newCtrIndex <- readMVar counters
    newCtrValue <- Counter.sample $ newCtrIndex ! cid
    Counter.unCounterSample newCtrValue `shouldBe` 0
    -- register should contain new counter metric
    newRegistrySample <- Registry.sample $ metricsRegistry state
    let registryMap = RSample.unRegistrySample newRegistrySample
        (Metric.CounterMetricSample registryCtrSample) = registryMap ! cMetricId
    Counter.unCounterSample registryCtrSample `shouldBe` 0
    -- increment
    _ <- eval state $ incCount countName
    let newCounters = metricsCounters state
    incCtrIndex <- readMVar newCounters
    incCtrValue <- Counter.sample $ incCtrIndex ! cid
    Counter.unCounterSample incCtrValue `shouldBe` 1

  let histName = HistogramName "blip" [] []
      h = histogramToIdentifier $ histName
      hName = fixMetricName $ metricIdName h
      hLabels = metricIdLabels h
      hid = metricIdStorable h
      hMetricIdName = MetricId.Name hName
      hMetricId = MetricId.MetricId hMetricIdName hLabels
  it "Can make a new histogram and observe it" $ do
    state <- emptyState
    -- new histogram
    _ <- eval state $ observeHistogram histName 0.0
    let histograms = metricsHistograms state
    newHistIndex <- readMVar histograms
    newHistValue <- Histogram.sample $ newHistIndex ! hid
    Histogram.histSum newHistValue `shouldBe` 0.0
    Histogram.histCount newHistValue `shouldBe` 0
    -- register should contain new histogram metric
    newRegistrySample <- Registry.sample $ metricsRegistry state
    let registryMap = RSample.unRegistrySample newRegistrySample
        (Metric.HistogramMetricSample registryHistSample) = registryMap ! hMetricId
    Histogram.histSum registryHistSample `shouldBe` 0.0
    Histogram.histCount registryHistSample `shouldBe` 0
    -- observe
    _ <- eval state $ observeHistogram histName 42.0
    let newHistograms = metricsHistograms state
    obsHistIndex <- readMVar newHistograms
    obsHistValue <- Histogram.sample $ obsHistIndex ! hid
    Histogram.histSum obsHistValue `shouldBe` 42.0
    Histogram.histCount obsHistValue `shouldBe` 1

  let buckets = [0.0001, 0.001, 0.01, 0.1, 0.25, 0.5, 0.75, 1, 10]
      buckettedHistName = HistogramName "buckets" [] buckets
      buckettedH = histogramToIdentifier $ buckettedHistName
      buckettedHName = fixMetricName $ metricIdName buckettedH
      buckettedHLabels = metricIdLabels buckettedH
      buckettedHMetricIdName = MetricId.Name buckettedHName
      buckettedHMetricId = MetricId.MetricId buckettedHMetricIdName buckettedHLabels
  it "Can measure action response times" $ do
    state <- emptyState
    -- create a new hist
    _ <- eval state $ observeHistogram buckettedHistName 0.0
    -- time an action
    (_, time) <- eval state $ withTimer buckettedHistName shine
    time `shouldSatisfy` (> 0)
    -- check hist buckets
    newRegistrySample <- Registry.sample $ metricsRegistry state
    let registryMap = RSample.unRegistrySample newRegistrySample
        (Metric.HistogramMetricSample registryHistSample) = registryMap ! buckettedHMetricId
        histBuckets = Histogram.histBuckets registryHistSample
    Map.elems histBuckets `shouldSatisfy` atLeastOneUpdated
      where atLeastOneUpdated = foldr (\b acc -> acc || (b /= 0.0)) False
