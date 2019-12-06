{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Metrics
  ( Metrics(..)
  , Severity(..)
  , MsgType(..)
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

data MsgType =
  MsgEcho | MsgInfo | MsgSetOption | MsgQuery | MsgCheckTx | MsgFlush | MsgInitChain | MsgBeginBlock | MsgDeliverTx | MsgEndBlock | MsgCommit
  deriving (Eq, Ord, Show)

data Severity =
  Debug | Info | Warning | Error | Exception
  deriving (Eq, Ord)

data Metrics m a where
  -- | Increments the count of a specific message
  IncCount :: MsgType -> Metrics m ()
  -- | Times an action
  WithTimer :: m a -> Metrics m (a, NominalDiffTime)

makeSem ''Metrics

evalMetrics
  :: Member (Embed IO) r
  => MVar (Map MsgType Integer)
  -> Sem (Metrics ': r) a
  -> Sem r a
evalMetrics mvarMap = do
  interpretH (\case
    IncCount msgType -> do
      liftIO $ modifyMVar_ mvarMap $ \countMap ->
        let newCount = maybe 1 (+1) (countMap !? msgType)
        in pure $ insert msgType newCount countMap
      pureT ()
    WithTimer action -> do
      startTime <- liftIO $ getCurrentTime
      a <- runT action
      endTime <- liftIO $ getCurrentTime
      let time = diffUTCTime endTime startTime
      actionRes <- raise $ evalMetrics mvarMap a
      pure $ (, time) <$> actionRes
    )
