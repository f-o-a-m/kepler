{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Test.GasSpec (spec) where

import           Control.Monad.IO.Class        (MonadIO (..))
import           Data.Either                   (isRight)
import qualified Data.IORef                    as Ref
import           Polysemy
import           Polysemy.Error                (Error, runError)
import           Polysemy.State                (State, runStateIORef)
import           Tendermint.SDK.BaseApp.Errors (AppError (..))
import qualified Tendermint.SDK.BaseApp.Gas    as G
import           Test.Hspec

data Dog m a where
    Bark :: Dog m ()

makeSem ''Dog

evalDog :: Sem (Dog ': r) a -> Sem r a
evalDog = interpret $ \case
  Bark -> pure ()

eval
  :: Ref.IORef G.GasAmount
  -> Sem [Dog, G.GasMeter, Error AppError, Embed IO] a
  -> IO (Either AppError a)
eval meter =
  runM .
    runError .
    runStateIORef meter .
    G.eval .
    raiseUnder @(State G.GasAmount) .
    evalDog

spec :: Spec
spec = describe "Gas Tests" $ do
    it "Can perform a computation without running out of gas"  $ do
      meter <- Ref.newIORef 1
      eRes <- eval meter $ G.withGas 1 bark
      eRes `shouldSatisfy` isRight
      remainingGas <- Ref.readIORef meter
      remainingGas `shouldBe` 0

    it "Can perform a computation with surplus gas"  $ do
      meter <- Ref.newIORef 2
      eRes <- eval meter $ G.withGas 1 bark
      eRes `shouldSatisfy` isRight
      remainingGas <- Ref.readIORef meter
      remainingGas `shouldBe` 1

    it "Can perform a computation and run out of gas"  $ do
      meter <- Ref.newIORef 0
      var <- Ref.newIORef (1 :: Int)
      eRes <- eval meter $ do
        G.withGas 1 bark
        -- this shouldn't execute
        liftIO $ Ref.modifyIORef var (+ 1)
      let AppError{..} = case eRes of
            Left e  -> e
            Right _ -> error "Was supposed to run out of gas"
      appErrorCode `shouldBe` 4
      remainingGas <- Ref.readIORef meter
      remainingGas `shouldBe` 0
      varVal <- Ref.readIORef var
      varVal `shouldBe` 1

