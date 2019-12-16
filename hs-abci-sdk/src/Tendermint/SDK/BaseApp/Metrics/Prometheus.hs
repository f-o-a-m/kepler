module Tendermint.SDK.BaseApp.Metrics.Prometheus where

import           Control.Arrow                                 ((***))
import           Control.Concurrent                            (forkIO)
import           Control.Concurrent.MVar                       (MVar,
                                                                modifyMVar_,
                                                                newMVar)
import           Control.Monad                                 (unless)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Data.Map.Strict                               (Map, insert)
import qualified Data.Map.Strict                               as Map
import           Data.Maybe                                    (fromJust,
                                                                isNothing)
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
-- eval
--------------------------------------------------------------------------------

evalWithMetrics
  :: Member (Embed IO) r
  => Member (Reader (Maybe MetricsConfig)) r
  => Sem (Metrics ': r) a
  -> Sem r a
evalWithMetrics action = do
  mCfg <- ask
  case mCfg of
    Nothing  -> evalNothing action
    Just cfg -> evalMetrics (metricsState cfg) action

evalNothing
  :: Sem (Metrics ': r) a
  -> Sem r a
evalNothing = do
  interpretH (\case
    IncCount _ -> pureT ()
    WithTimer _ _ -> pureT ()
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
      liftIO $ modifyMVar_ metricsCounters $ \counterMap ->
        case Map.lookup cid counterMap of
          Nothing -> do
            newCtr <- liftIO $
              Registry.registerCounter cMetricIdName metricIdLabels metricsRegistry
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
      _ <- raise $ evalMetrics state a
      pureT ()
    )

-- | Updates a histogram with an observed value
observeHistogram :: MonadIO m => MetricsState -> HistogramName -> Double -> m ()
observeHistogram MetricsState{..} histName val = do
  let h@MetricIdentifier{..} = histogramToIdentifier histName
      hid = metricIdStorable h
      hMetricIdName = MetricId.Name metricIdName
  liftIO $ modifyMVar_ metricsHistograms $ \histMap ->
    case Map.lookup hid histMap of
      Nothing -> do
        newHist <- liftIO $
          Registry.registerHistogram hMetricIdName metricIdLabels metricIdHistoBuckets metricsRegistry
        let newHistMap = insert hid newHist histMap
        liftIO $ Histogram.observe val newHist
        pure $ newHistMap
      Just hist -> do
        liftIO $ Histogram.observe val hist
        pure histMap

--------------------------------------------------------------------------------
-- indexes
--------------------------------------------------------------------------------

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

fixMetricName :: Text -> Text
fixMetricName = Text.map fixer
  where fixer c = if c `elem` validChars then c else '_'
        validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

-- | Core metrics state
type MetricsMap a = Map (Text, MetricId.Labels) a
data MetricsState = MetricsState
  { metricsRegistry   :: Registry.Registry
  , metricsCounters   :: MVar (MetricsMap Counter.Counter)
  , metricsHistograms :: MVar (MetricsMap Histogram.Histogram)
  }

-- | Core metrics config
type Port = Int
data MetricsConfig = MetricsConfig
  { metricsState  :: MetricsState
  , metricsPort   :: Maybe Port
  , metricsAPIKey :: Maybe Text
  }

-- | Intermediary prometheus registry index key
data MetricIdentifier = MetricIdentifier
  { metricIdName         :: Text
  , metricIdLabels       :: MetricId.Labels
  , metricIdHistoBuckets :: [Double]
  }

instance IsString MetricIdentifier where
  fromString s = MetricIdentifier (fromString s) mempty mempty

--------------------------------------------------------------------------------
-- setup
--------------------------------------------------------------------------------

emptyState :: IO MetricsState
emptyState = do
  counters <- newMVar Map.empty
  histos <- newMVar Map.empty
  registry <- Registry.new
  return $ MetricsState registry counters histos

-- | Default metrics cfg
-- | Port defaults to 9200
mkMetricsConfig :: IO MetricsConfig
mkMetricsConfig = do
  state <- emptyState
  return $ MetricsConfig state (Just 9200) Nothing

runMetricsServer
  :: MonadIO m
  => Maybe MetricsConfig
  -> m ()
runMetricsServer mMetCfg = liftIO $ do
  unless (isNothing mMetCfg) $ do
    let MetricsConfig{..} = fromJust mMetCfg
        MetricsState{..} = metricsState
    unless (isNothing metricsPort) $ do
      _ <- forkIO $
        Http.serveHttpTextMetrics (fromJust metricsPort) ["metrics"] (Registry.sample metricsRegistry)
      return ()
