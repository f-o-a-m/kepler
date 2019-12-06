{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.MetricsSpec where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Data.Map.Strict         (Map, empty, (!?))
import           Polysemy
import qualified Tendermint.SDK.Metrics  as Met
import           Test.Hspec

data Fox m a where
  Shine :: Fox m ()

makeSem ''Fox

evalFox :: Sem (Fox ': r) a -> Sem r a
evalFox = interpret $ \case
  Shine -> pure ()

eval
  :: MVar (Map Met.CountName Integer)
  -> Sem [Fox, Met.Metrics, Embed IO] a
  -> IO a
eval mvarMap = runM . Met.evalMetrics mvarMap . evalFox

spec :: Spec
spec = describe "Metrics tests" $ do
  it "Can increment counts" $ do
    let countName = Met.CountName "blip"
    mvarMap <- newMVar empty
    _ <- eval mvarMap $ Met.incCount countName
    newMap <- readMVar mvarMap
    newMap !? countName `shouldBe` Just 1

  it "Can measure action response times" $ do
    mvarMap <- newMVar empty
    (_, time) <- eval mvarMap $ Met.withTimer shine
    time `shouldSatisfy` (> 0)
