{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.BaseApp.Metrics.Prometheus
  ( -- config and setup
    MetricsScrapingConfig(..)
  , prometheusPort
  , MetricsState(..)
  , metricsRegistry
  , metricsCounters
  , metricsHistograms
  , PrometheusEnv(..)
  , envMetricsState
  , envMetricsScrapingConfig
  , emptyState
  , forkMetricsServer
  -- utils
  , mkPrometheusMetricId
  , metricIdStorable
  , countToIdentifier
  , histogramToIdentifier
  -- eval
  , evalWithMetrics
  , evalNothing
  , evalMetrics
  ) where

import           Control.Arrow                                 ((***))
import           Control.Concurrent                            (ThreadId,
                                                                forkIO)
import           Control.Concurrent.MVar                       (MVar,
                                                                modifyMVar_,
                                                                newMVar)
import           Control.Lens                                  (makeLenses)
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
import           Polysemy.Reader                               (Reader (..),
                                                                ask)
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import qualified System.Metrics.Prometheus.Http.Scrape         as Http
import qualified System.Metrics.Prometheus.Metric.Counter      as Counter
import qualified System.Metrics.Prometheus.Metric.Histogram    as Histogram
import qualified System.Metrics.Prometheus.MetricId            as MetricId
import           Tendermint.SDK.BaseApp.Metrics                (CountName (..), HistogramName (..),
                                                                Metrics (..))
--------------------------------------------------------------------------------
-- Metrics Types
--------------------------------------------------------------------------------

-- | Core metrics state
type MetricsMap a = Map (Text, MetricId.Labels) a

data MetricsState = MetricsState
  { _metricsRegistry   :: Registry.Registry
  , _metricsCounters   :: MVar (MetricsMap Counter.Counter)
  , _metricsHistograms :: MVar (MetricsMap Histogram.Histogram)
  }
makeLenses ''MetricsState

-- | Intermediary prometheus registry index key
data MetricIdentifier = MetricIdentifier
  { metricIdName         :: Text
  , metricIdLabels       :: MetricId.Labels
  , metricIdHistoBuckets :: [Double]
  }

instance IsString MetricIdentifier where
  fromString s = MetricIdentifier (fromString s) mempty mempty

fixMetricName :: Text -> Text
fixMetricName = Text.map fixer
  where fixer c = if c `elem` validChars then c else '_'
        validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

-- indexes

countToIdentifier :: CountName -> MetricIdentifier
countToIdentifier (CountName name labels) = MetricIdentifier
  { metricIdName = fixMetricName name
  , metricIdLabels = MetricId.fromList labels
  , metricIdHistoBuckets = []
  }

histogramToIdentifier :: HistogramName -> MetricIdentifier
histogramToIdentifier (HistogramName name labels buckets) = MetricIdentifier
  { metricIdName = fixMetricName name
  , metricIdLabels = MetricId.fromList labels
  , metricIdHistoBuckets = buckets
  }

-- | Prometheus registry index key
mkPrometheusMetricId :: MetricIdentifier -> MetricId.MetricId
mkPrometheusMetricId MetricIdentifier{..} =
  MetricId.MetricId (MetricId.Name metricIdName) metricIdLabels

-- | Index key for storing metrics
metricIdStorable :: MetricIdentifier -> (Text, MetricId.Labels)
metricIdStorable c = (fixMetricName $ metricIdName c, fixMetricLabels $ metricIdLabels c)
  where fixMetricLabels =
          MetricId.fromList .
          map (fixMetricName *** fixMetricName) .
          MetricId.toList


--------------------------------------------------------------------------------
-- Config
--------------------------------------------------------------------------------

-- | Core metrics config
data MetricsScrapingConfig = MetricsScrapingConfig
  { _prometheusPort :: Int
  }

makeLenses ''MetricsScrapingConfig

data PrometheusEnv = PrometheusEnv
  { _envMetricsState          :: MetricsState
  , _envMetricsScrapingConfig :: MetricsScrapingConfig
  }

makeLenses ''PrometheusEnv

emptyState :: IO MetricsState
emptyState = do
  counters <- newMVar Map.empty
  histos <- newMVar Map.empty
  registry <- Registry.new
  return $ MetricsState registry counters histos

forkMetricsServer
  :: MonadIO m
  => PrometheusEnv
  -> m ThreadId
forkMetricsServer metCfg = liftIO $
  let PrometheusEnv{..} = metCfg
      port = _prometheusPort $ _envMetricsScrapingConfig
      MetricsState{..} = _envMetricsState
  in forkIO $ Http.serveHttpTextMetrics port ["metrics"] (Registry.sample _metricsRegistry)

--------------------------------------------------------------------------------
-- eval
--------------------------------------------------------------------------------

evalWithMetrics
  :: Member (Embed IO) r
  => Member (Reader (Maybe PrometheusEnv)) r
  => Sem (Metrics ': r) a
  -> Sem r a
evalWithMetrics action = do
  mCfg <- ask
  case mCfg of
    Nothing  -> evalNothing action
    Just cfg -> evalMetrics (_envMetricsState cfg) action

evalNothing
  :: Sem (Metrics ': r) a
  -> Sem r a
evalNothing = do
  interpretH (\case
    IncCount _ -> pureT ()
    WithTimer _ action -> do
      a <- runT action
      raise $ evalNothing a
    )

evalMetrics
  :: Member (Embed IO) r
  => MetricsState
  -> Sem (Metrics ': r) a
  -> Sem r a
evalMetrics state@MetricsState{..} = do
  interpretH (\case
    -- | Increments existing count; if it doesn't exist, creates a new
    -- | counter and increments it.
    IncCount ctrName -> do
      let c@MetricIdentifier{..} = countToIdentifier ctrName
          cid = metricIdStorable c
          cMetricIdName = MetricId.Name metricIdName
      liftIO $ modifyMVar_ _metricsCounters $ \counterMap ->
        case Map.lookup cid counterMap of
          Nothing -> do
            newCtr <- liftIO $
              Registry.registerCounter cMetricIdName metricIdLabels _metricsRegistry
            let newCounterMap = insert cid newCtr counterMap
            liftIO $ Counter.inc newCtr
            pure newCounterMap
          Just ctr -> do
            liftIO $ Counter.inc ctr
            pure counterMap
      pureT ()

    -- | Updates a histogram with the time it takes to do an action
    -- | If histogram doesn't exist, creates a new one and observes it.
    WithTimer histName action -> do
      start <- liftIO $ getCurrentTime
      a <- runT action
      end <- liftIO $ getCurrentTime
      let time = fromRational . (* 1000.0) . toRational $ (end `diffUTCTime` start)
      observeHistogram state histName time
      raise $ evalMetrics state a
    )

-- | Updates a histogram with an observed value
observeHistogram :: MonadIO m => MetricsState -> HistogramName -> Double -> m ()
observeHistogram MetricsState{..} histName val = liftIO $ do
  let h@MetricIdentifier{..} = histogramToIdentifier histName
      hid = metricIdStorable h
      hMetricIdName = MetricId.Name metricIdName
  modifyMVar_ _metricsHistograms $ \histMap ->
    case Map.lookup hid histMap of
      Nothing -> do
        newHist <-
          Registry.registerHistogram hMetricIdName metricIdLabels metricIdHistoBuckets _metricsRegistry
        let newHistMap = insert hid newHist histMap
        Histogram.observe val newHist
        pure $ newHistMap
      Just hist -> do
        Histogram.observe val hist
        pure histMap
