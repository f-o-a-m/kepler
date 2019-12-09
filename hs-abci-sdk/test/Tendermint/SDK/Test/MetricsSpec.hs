{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.MetricsSpec where

import           Control.Concurrent.MVar            (newMVar)
import qualified Data.Map.Strict                    as Map
import           Polysemy
import qualified System.Metrics.Prometheus.Registry as Registry
import qualified Tendermint.SDK.Metrics             as Eff
import qualified Tendermint.SDK.Metrics.Metrics     as Met
import           Test.Hspec

data Fox m a where
  Shine :: Fox m ()

makeSem ''Fox

evalFox :: Sem (Fox ': r) a -> Sem r a
evalFox = interpret $ \case
  Shine -> pure ()

eval
  :: Met.MetricsRegistry
  -> Sem [Fox, Eff.Metrics, Embed IO] a
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
  it "Can increment counts" $ do
    pending
    -- let countName = Met.CountName "blip"
    -- mvarMap <- newMVar empty
    -- _ <- eval mvarMap $ Met.incCount countName
    -- newMap <- readMVar mvarMap
    -- newMap !? countName `shouldBe` Just 1

  it "Can measure action response times" $ do
    registry <- emptyRegistry
    (_, time) <- eval registry $ Eff.withTimer (Eff.HistogramName "blip") shine
    time `shouldSatisfy` (> 0)
