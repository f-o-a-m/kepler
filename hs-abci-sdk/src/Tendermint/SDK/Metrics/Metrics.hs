{-# LANGUAGE TupleSections #-}

module Tendermint.SDK.Metrics.Metrics where

import           Control.Arrow                                 ((***))
import           Control.Concurrent                            (forkIO)
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
import           System.Environment                            (lookupEnv)
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import qualified System.Metrics.Prometheus.Http.Scrape         as Http
import qualified System.Metrics.Prometheus.Metric.Counter      as Counter
import qualified System.Metrics.Prometheus.Metric.Histogram    as Histogram
import qualified System.Metrics.Prometheus.MetricId            as MetricId
import           Tendermint.SDK.Metrics                        (CountName (..), HistogramName (..),
                                                                Metrics (..),
                                                                observeHistogram)
import qualified Text.Read                                     as T

evalMetrics
  :: Member (Embed IO) r
  => MetricsState
  -> Sem (Metrics ': r) a
  -> Sem r a
evalMetrics state = do
  interpretH (\case
    IncCount ctrName -> do
      let c = countToIdentifier ctrName
          cName = fixMetricName $ metricIdName c
          cLabels = metricIdLabels c
          cid = metricIdStorable c
          cMetricIdName = MetricId.Name cName
          registry = metricsRegistry state
          counters = metricsCounters state
      liftIO $ modifyMVar_ counters $ \counterMap ->
        case Map.lookup cid counterMap of
          Nothing -> do
            newCtr <-
              liftIO $ Registry.registerCounter cMetricIdName cLabels registry
            pure $ insert cid newCtr counterMap
          Just ctr -> do
            liftIO $ Counter.inc ctr
            pure counterMap
      pureT ()

    ObserveHistogram histName val -> do
      let h = histogramToIdentifier histName
          hName = fixMetricName $ metricIdName h
          hLabels = metricIdLabels h
          hBuckets = metricIdHistoBuckets h
          hid = metricIdStorable h
          hMetricIdName = MetricId.Name hName
          registry = metricsRegistry state
          histograms = metricsHistograms state
      liftIO $ modifyMVar_ histograms $ \histMap ->
        case Map.lookup hid histMap of
          Nothing -> do
            newHist <-
              liftIO $ Registry.registerHistogram hMetricIdName hLabels hBuckets registry
            pure $ insert hid newHist histMap
          Just hist -> do
            liftIO $ Histogram.observe val hist
            pure histMap
      pureT ()

    WithTimer histName action -> do
      start <- liftIO $ getCurrentTime
      a <- runT action
      end <- liftIO $ getCurrentTime
      let time = fromRational . (* 1000.0) . toRational $ (end `diffUTCTime` start)
      evalMetrics state (observeHistogram histName time)
      actionRes <- raise $ evalMetrics state a
      pure $ (, time) <$> actionRes
    )

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

countToIdentifier :: CountName -> MetricIdentifier
countToIdentifier (CountName name labels) = MetricIdentifier
  { metricIdName = name
  , metricIdLabels = MetricId.fromList labels
  , metricIdHistoBuckets = []
  }

histogramToIdentifier :: HistogramName -> MetricIdentifier
histogramToIdentifier (HistogramName name labels buckets) = MetricIdentifier
  { metricIdName = name
  , metricIdLabels = MetricId.fromList labels
  , metricIdHistoBuckets = buckets
  }

getEnvVarBoolWithDefault :: MonadIO m => String -> Bool -> m Bool
getEnvVarBoolWithDefault var def = liftIO (lookupEnv var) >>= \case
  Nothing -> return def
  Just _ -> undefined

readEnvVarWithDefault :: (Read a, MonadIO m) => String -> a -> m a
readEnvVarWithDefault var def =
  liftIO (lookupEnv var) >>= \case
    Nothing -> return def
    Just s -> return $ T.read s

initMetricsState
  :: MonadIO m
  => m (Maybe MetricsState)
initMetricsState = liftIO $ do
  shouldStart <- getEnvVarBoolWithDefault "STATS_ENABLED" True
  if not shouldStart
    then return Nothing
    else do
      bindPort <- readEnvVarWithDefault "STATS_PORT" 9200
      counters <- newMVar Map.empty
      histos <- newMVar Map.empty
      registry <- Registry.new
      _ <- forkIO $ Http.serveHttpTextMetrics bindPort ["metrics"] (Registry.sample registry)
      return . Just $ MetricsState registry counters histos
