module Network.ABCI.Server.Middleware.Metrics
    ( defaultBuckets
    , mkMetricsMiddleware
    ) where

import           Control.Monad                                 (forM_)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import qualified Data.IORef                                    as Ref
import qualified Data.Map.Strict                               as Map
import           Data.String.Conversions                       (cs)
import           Data.Time                                     (diffUTCTime,
                                                                getCurrentTime)
import           Network.ABCI.Server.App                       (App (..),
                                                                MessageType (..),
                                                                Middleware,
                                                                demoteRequestType,
                                                                msgTypeKey)
import qualified System.Metrics.Prometheus.Concurrent.Registry as Registry
import qualified System.Metrics.Prometheus.Metric.Counter      as Counter
import qualified System.Metrics.Prometheus.Metric.Histogram    as Histogram
import qualified System.Metrics.Prometheus.MetricId            as MetricId

---------------------------------------------------------------------------
-- mkMetrics
---------------------------------------------------------------------------
-- | Metrics logger middleware for ABCI server already within the KatipContext.
-- Great for `App m` with a `KatipContext` instance.

mkMetricsMiddleware
  :: MonadIO m
  => [Histogram.UpperBound]
  -> Registry.Registry
  -> IO (Middleware m)
mkMetricsMiddleware buckets registry = do
  Config{..} <- makeConfig buckets registry
  return $ \(App app) -> App $ \ req -> do
       startTime <- liftIO getCurrentTime
       res <- app req
       endTime <- liftIO getCurrentTime
       let msgType = demoteRequestType req
           duration = realToFrac $ diffUTCTime endTime startTime
       liftIO $ do
         incRequestCounter cfgCounterMap msgType
         addToHistogram cfgHistogramMap msgType duration
       pure res
  where

    incRequestCounter counterMapRef msgType = do
      counter <- do
        counterMap <- Ref.readIORef counterMapRef
        case Map.lookup msgType counterMap of
          Nothing -> error $ "Impossible missing counter for " <> msgTypeKey msgType
          Just c -> return c
      Counter.inc counter

    addToHistogram histogramMapRef msgType duration = do
      histogram <- do
        histMap <- Ref.readIORef histogramMapRef
        case Map.lookup msgType histMap of
          Nothing -> error $ "Impossible missing histogram for " <> msgTypeKey msgType
          Just c -> return c
      Histogram.observe duration histogram

data Config = Config
  { cfgRegistry         :: Registry.Registry
  , cfgHistogramBuckets :: [Histogram.UpperBound]
  , cfgCounterMap       :: Ref.IORef (Map.Map MessageType Counter.Counter)
  , cfgHistogramMap     :: Ref.IORef (Map.Map MessageType Histogram.Histogram)
  }

makeConfig
  :: [Histogram.UpperBound]
  -> Registry.Registry
  -> IO Config
makeConfig bounds registry = do
  counterMap <- Ref.newIORef Map.empty
  histMap <- Ref.newIORef Map.empty
  let cfg = Config
        { cfgRegistry = registry
        , cfgHistogramBuckets = bounds
        , cfgCounterMap = counterMap
        , cfgHistogramMap = histMap
        }
  registerMetrics cfg
  return cfg

registerMetrics
  :: Config
  -> IO ()
registerMetrics Config{..} = do
  registerHistograms cfgHistogramBuckets cfgRegistry cfgHistogramMap
  registerCounters cfgRegistry cfgCounterMap
  where

    registerHistograms
      :: [Histogram.UpperBound]
      -> Registry.Registry
      -> Ref.IORef (Map.Map MessageType Histogram.Histogram)
      -> IO ()
    registerHistograms buckets registry histRef =
      let histName = "abci_request_duration_seconds"
      in forM_ [MTEcho .. MTCommit] $ \messageType  -> do
           let labels = MetricId.Labels . Map.fromList $
                 [ ("message_type", cs $ msgTypeKey messageType)
                 ]
           hist <- Registry.registerHistogram histName labels buckets registry
           Ref.modifyIORef' histRef (Map.insert messageType hist)


    registerCounters
      :: Registry.Registry
      -> Ref.IORef (Map.Map MessageType Counter.Counter)
      -> IO ()
    registerCounters registry counterRef =
      let counterName = "abci_request_total"
      in forM_ [MTEcho .. MTCommit] $ \messageType  -> do
           let labels = MetricId.Labels . Map.fromList $
                 [ ("message_type", cs $ msgTypeKey messageType)
                 ]
           counter <- Registry.registerCounter counterName labels registry
           Ref.modifyIORef' counterRef (Map.insert messageType counter)

-- buckets with upper bounds [0.005, 0.01, 0.015 ... 5.0]
-- measured in seconds
defaultBuckets :: [Histogram.UpperBound]
defaultBuckets = [0.0001, 0.0005, 0.001, 0.005, 0.01, 0.05, 0.1, 0.5, 1.0, 5.0, 10.0]
