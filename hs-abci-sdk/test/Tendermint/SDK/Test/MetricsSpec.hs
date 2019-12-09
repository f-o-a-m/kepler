{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.MetricsSpec where

import           Control.Concurrent.MVar                  (newMVar, readMVar)
import           Data.Map.Strict                          ((!))
import qualified Data.Map.Strict                          as Map
import           Data.String                              (fromString)
import           Data.Text                                (unpack)
import           Polysemy
import qualified System.Metrics.Prometheus.Metric.Counter as Counter
import qualified System.Metrics.Prometheus.Registry       as Registry
import           Tendermint.SDK.Metrics
import qualified Tendermint.SDK.Metrics.Metrics           as Met
import           Test.Hspec

data Fox m a where
  Shine :: Fox m ()

makeSem ''Fox

evalFox :: Sem (Fox ': r) a -> Sem r a
evalFox = interpret $ \case
  Shine -> pure ()

eval
  :: Met.MetricsRegistry
  -> Sem [Fox, Metrics, Embed IO] a
  -> IO a
eval registry = runM . Met.evalMetrics registry . evalFox

emptyRegistry :: IO Met.MetricsRegistry
emptyRegistry = do
  counters <- newMVar Map.empty
  histos <- newMVar Map.empty
  registry <- newMVar Registry.new
  return $ Met.MetricsRegistry registry counters histos

spec :: Spec
spec = describe "Metrics tests" $ do
  it "Can measure action response times" $ do
    let histName = HistogramName "blip"
    registry <- emptyRegistry
    (_, time) <- eval registry $ withTimer histName shine
    time `shouldSatisfy` (> 0)

  it "Can increment counts" $ do
    let countName = CountName "blip"
        c = fromString . unpack . unCountName $ countName
        cid = Met.metricIdStorable c
    registry@Met.MetricsRegistry {..} <- emptyRegistry
    _ <- eval registry $ incCount countName
    newMap <- readMVar metricsCounters
    ctrValue <- Counter.sample (newMap ! cid)
    Counter.unCounterSample ctrValue `shouldBe` 1
