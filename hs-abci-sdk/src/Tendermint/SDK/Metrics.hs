{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Metrics
  ( Metrics(..)
  , CountName(..)
  , incCount
  , withTimer
  , evalMetrics
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Map.Strict         (Map, insert, (!?))
import           Data.Time               (NominalDiffTime, diffUTCTime, getCurrentTime)
import           Polysemy                (Embed, Member, Sem, interpretH,
                                          makeSem, pureT, raise, runT)
import Data.Text (Text)

newtype CountName = CountName { unCountName :: Text }
  deriving (Eq, Ord)

data Metrics m a where
  -- | Increments the count of a specific message
  IncCount :: CountName -> Metrics m ()
  -- | Times an action
  WithTimer :: m a -> Metrics m (a, NominalDiffTime)

makeSem ''Metrics

evalMetrics
  :: Member (Embed IO) r
  => MVar (Map CountName Integer)
  -> Sem (Metrics ': r) a
  -> Sem r a
evalMetrics mvarMap = do
  interpretH (\case
    IncCount countName -> do
      liftIO $ modifyMVar_ mvarMap $ \countMap ->
        let newCount = maybe 1 (+1) (countMap !? countName)
        in pure $ insert countName newCount countMap
      pureT ()
    WithTimer action -> do
      startTime <- liftIO $ getCurrentTime
      a <- runT action
      endTime <- liftIO $ getCurrentTime
      let time = diffUTCTime endTime startTime
      actionRes <- raise $ evalMetrics mvarMap a
      pure $ (, time) <$> actionRes
    )
