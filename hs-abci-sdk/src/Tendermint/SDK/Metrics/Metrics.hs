{-# LANGUAGE TupleSections #-}

module Tendermint.SDK.Metrics.Metrics where

import           Control.Arrow                                 ((***))
import           Control.Concurrent.MVar                       (MVar,
                                                                modifyMVar_,
                                                                newMVar)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Data.Map.Strict                               (Map, insert)
import qualified Data.Map.Strict                               as Map
import           Data.String                                   (IsString,
                                                                fromString)
import           Data.Text                                     (Text)
import qualified Data.Text                                     as Text
import           Data.Time                                     (diffUTCTime,
                                                                getCurrentTime)
import           Polysemy                                      (Embed, Member,
                                                                Sem, interpretH,
                                                                pureT, raise,
                                                                runT)
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import qualified System.Metrics.Prometheus.Metric.Counter      as Counter
import qualified System.Metrics.Prometheus.Metric.Histogram    as Histogram
import qualified System.Metrics.Prometheus.MetricId            as MetricId
import           Tendermint.SDK.Metrics                        (CountName (..),
                                                                Metrics (..))
--import System.Environment (lookupEnv)

type MetricsMap a = Map (Text, MetricId.Labels) a
data MetricsState = MetricsState
  { metricsRegistry   :: Registry.Registry
  , metricsCounters   :: MVar (MetricsMap Counter.Counter)
  , metricsHistograms :: MVar (MetricsMap Histogram.Histogram)
  }

data MetricIdentifier = MetricIdentifier
  { metricIdName         :: Text
  , metricIdLabels       :: MetricId.Labels
  , metricIdHistoBuckets :: [Double]
  }

instance IsString MetricIdentifier where
  fromString s = MetricIdentifier (fromString s) mempty mempty

metricIdStorable :: MetricIdentifier -> (Text, MetricId.Labels)
metricIdStorable c = (fixMetricName $ metricIdName c, fixMetricLabels $ metricIdLabels c)
  where fixMetricLabels =
          MetricId.fromList .
          map (fixMetricName *** fixMetricName) .
          MetricId.toList

fixMetricName :: Text -> Text
fixMetricName = Text.map fixer
  where fixer c = if c `elem` validChars then c else '_'
        validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

evalMetrics
  :: Member (Embed IO) r
  => MetricsState
  -> Sem (Metrics ': r) a
  -> Sem r a
evalMetrics state = do
  interpretH (\case
    IncCount ctrName -> do
      let c = fromString . Text.unpack . unCountName $ ctrName
          cName = fixMetricName $ metricIdName c
          cLabels = metricIdLabels c
          cid = metricIdStorable c
          registry = metricsRegistry state
          counters = metricsCounters state
      liftIO $ modifyMVar_ counters $ \counterMap ->
        case Map.lookup cid counterMap of
          Nothing -> do
            newCtr <-
              liftIO $ Registry.registerCounter (MetricId.Name cName) cLabels registry
            pure $ insert cid newCtr counterMap
          Just ctr -> do
            liftIO $ Counter.inc ctr
            pure counterMap
      pureT ()

    ObserveHistogram _ _ -> do
      undefined

    WithTimer _ action -> do
      startTime <- liftIO $ getCurrentTime
      a <- runT action
      endTime <- liftIO $ getCurrentTime
      let time = diffUTCTime endTime startTime
      -- @TODO: update a histogram here
      actionRes <- raise $ evalMetrics state a
      pure $ (, time) <$> actionRes
    )

-- getEnvVarWithDefault :: MonadIO m => String -> String -> m String
-- getEnvVarWithDefault var def = undefined
-- getEnvVarBoolWithDefault :: MonadIO m => String -> Bool -> m Bool
-- getEnvVarBoolWithDefault var def = undefined
-- readEnvVarWithDefault var def = undefined

initMetricsState
  :: MonadIO m
  => m (Maybe MetricsState)
initMetricsState = liftIO $ do
  -- shouldStart <- getEnvVarBoolWithDefault "STATS_ENABLED" True
  -- if not False -- shouldStart
  --   then return Nothing
  --   else do
      -- bindAddr <- fromString <$> getEnvVarWithDefault "STATS_ADDR" "0.0.0.0"
      -- bindPort <- readEnvVarWithDefault "STATS_PORT" 9200
      -- logNotice "tcr.metrics" $ "Running Prometheus on " <> C8.unpack bindAddr <> ":" <> show bindPort
      counters <- newMVar Map.empty
      histos <- newMVar Map.empty
      -- _ <- fork $ Prometheus.Http.serveHttpTextMetrics bindPort ["metrics"] (Prometheus.Registry.sample registry)
      registry <- Registry.new
      return . Just $ MetricsState registry counters histos
