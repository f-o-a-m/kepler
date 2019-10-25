{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Logger where

import qualified Data.Map as M
import Data.Conduit
import qualified Data.IORef as IORef
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (forever, forM_)
import Control.Concurrent.Async as Async
import Control.Monad.IO.Class (MonadIO(..))
import Polysemy

data Logger m a where
  Log :: String -> Logger m ()

makeSem ''Logger

data SubscriptionDriver = SubscriptionDriver
  { listenersVar :: IORef.IORef (M.Map Int (MVar.MVar String))
  , freshVar :: IORef.IORef Int
  }

initSubscriptionDriver :: IO SubscriptionDriver
initSubscriptionDriver = do
  ls <- IORef.newIORef M.empty
  f <- IORef.newIORef 0
  pure $ SubscriptionDriver ls f

subscriptionEval
  :: MonadIO (Sem r)
  => SubscriptionDriver
  -> Sem (Logger ': r) a
  -> Sem r a
subscriptionEval SubscriptionDriver{listenersVar} = do
  interpret (\case
    Log msg -> liftIO $ do
      listeners <- IORef.readIORef listenersVar
      forM_ listeners $ \listener -> MVar.putMVar listener msg
    )

subscribe
  :: SubscriptionDriver
  -> ConduitT String Void IO ()
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
    mkProducer :: MonadIO m => MVar.MVar String -> ConduitT () String m ()
    mkProducer var = forever $ do
      mInput <- liftIO $ MVar.tryTakeMVar var
      case mInput of
        Nothing -> pure ()
        Just a  -> yield a
