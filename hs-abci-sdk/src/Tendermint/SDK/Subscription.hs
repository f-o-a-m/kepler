module Tendermint.SDK.Subscription where

import           Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar  as MVar
import           Control.Monad            (forM_, forever)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Data.Conduit
import qualified Data.IORef               as IORef
import qualified Data.Map                 as M
import           Polysemy
import           Polysemy.Output

data SubscriptionDriver o = SubscriptionDriver
  { listenersVar :: IORef.IORef (M.Map Int (MVar.MVar o))
  , freshVar     :: IORef.IORef Int
  }

initSubscriptionDriver :: IO (SubscriptionDriver o)
initSubscriptionDriver = do
  ls <- IORef.newIORef M.empty
  f <- IORef.newIORef 0
  pure $ SubscriptionDriver ls f

eval
  :: MonadIO (Sem r)
  => SubscriptionDriver o
  -> Sem (Output o ': r) a
  -> Sem r a
eval SubscriptionDriver{listenersVar} =
  interpret (\case
    Output o -> liftIO $ do
      listeners <- IORef.readIORef listenersVar
      forM_ listeners $ \listener -> MVar.putMVar listener o
    )

subscribe
  :: SubscriptionDriver o
  -> ConduitT o Void IO ()
  -> IO (Async.Async ())
subscribe SubscriptionDriver{freshVar, listenersVar} consumer = do
  inputVar <- MVar.newEmptyMVar
  listenerId <- do
    listenerId <- IORef.readIORef freshVar
    IORef.modifyIORef freshVar (1 +)
    IORef.modifyIORef listenersVar (M.insert listenerId inputVar)
    pure listenerId
  let producer = mkProducer inputVar
  Async.async $ do
    runConduit (producer .| consumer)
    IORef.modifyIORef listenersVar (M.delete listenerId)
  where
    mkProducer :: MonadIO m => MVar.MVar o -> ConduitT () o m ()
    mkProducer var = forever $ do
      mInput <- liftIO $ MVar.tryTakeMVar var
      case mInput of
        Nothing -> pure ()
        Just a  -> yield a
