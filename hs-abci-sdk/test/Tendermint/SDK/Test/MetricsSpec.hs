{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.MetricsSpec where

import           Control.Concurrent.MVar                       (readMVar)
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
    -- Creates new count and sets it to 1
    _ <- eval state $ incCount countName
    newCtrIndex <- readMVar metricsCounters
    newCtrValue <- Counter.sample $ newCtrIndex ! cid
    Counter.unCounterSample newCtrValue `shouldBe` 1
    -- register should contain new counter metric
    newRegistrySample <- Registry.sample metricsRegistry
    let registryMap = RSample.unRegistrySample newRegistrySample
        (Metric.CounterMetricSample registryCtrSample) = registryMap ! cMetricId
    Counter.unCounterSample registryCtrSample `shouldBe` 1
    -- increment it again
    _ <- eval state $ incCount countName
    incCtrIndex <- readMVar metricsCounters
    incCtrValue <- Counter.sample $ incCtrIndex ! cid
    Counter.unCounterSample incCtrValue `shouldBe` 2

  let buckettedHistName = "bucketted"
      buckettedH = histogramToIdentifier buckettedHistName
      buckettedHMetricId = mkPrometheusMetricId buckettedH
  it "Can measure action response times with default buckets" $ do
    state@MetricsState{..} <- emptyState
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
