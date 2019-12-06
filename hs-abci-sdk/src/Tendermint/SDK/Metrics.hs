{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Metrics
  ( Metrics(..)
  , Severity(..)
  , MsgType(..)
  , incCount
  , withTimer
  , Tendermint.SDK.Metrics.log
  , evalMetrics
  ) where

import           Control.Concurrent.MVar (MVar, modifyMVar_)
import           Control.Monad.IO.Class  (liftIO)
import           Data.Map.Strict         (Map, insert, (!?))
import           Data.String             (fromString)
import           Data.String.Conversions (cs)
import           Data.Text               (Text)
import           Data.Time               (diffUTCTime, getCurrentTime)
import qualified Katip                   as K
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
  WithTimer :: m a -> Metrics m a
  -- | Log metrics
  Log :: Severity -> Text -> Metrics m ()

makeSem ''Metrics

evalMetrics
  :: forall r a.
     K.KatipContext (Sem r)
  => Member (Embed IO) r
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
      -- @TODO: what do we do with this?
      let _ = diffUTCTime endTime startTime
      raise $ evalMetrics mvarMap a
    Log severity msg -> do
      raise $ K.logFM (coerceSeverity severity) (fromString . cs $ msg)
      pureT ()
    )
    where
      coerceSeverity :: Severity -> K.Severity
      coerceSeverity = \case
        Debug -> K.DebugS
        Info -> K.InfoS
        Warning -> K.WarningS
        Error -> K.ErrorS
        Exception -> K.CriticalS
