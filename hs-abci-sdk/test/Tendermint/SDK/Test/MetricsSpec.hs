{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.MetricsSpec where

import           Control.Concurrent.MVar (MVar, newMVar, readMVar)
import           Data.Map.Strict         (Map, empty, (!?))
-- import qualified Katip                   as K
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
  :: -- forall r a.
  --    K.KatipContext (Sem r)
  -- =>
  MVar (Map Met.MsgType Integer)
  -> Sem [Fox, Met.Metrics, Embed IO] a
  -> IO a
eval mvarMap = runM . Met.evalMetrics mvarMap . evalFox

spec :: Spec
spec = describe "Metrics tests" $ do
  it "Can increment message type counts" $ do
    mvarMap <- newMVar empty
    _ <- eval mvarMap $ Met.incCount Met.MsgFlush
    newMap <- readMVar mvarMap
    newMap !? Met.MsgFlush `shouldBe` Just 1
  -- it "Can time actions" $ do
  --   pending
