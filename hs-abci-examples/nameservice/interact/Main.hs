module Main where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad            (forever, replicateM)
import           Data.Maybe               (maybe)
import           Interact
import           System.Environment       (lookupEnv)
import           Text.Read                (read)

main :: IO ()
main = do
  mThreads <- lookupEnv "INTERACT_THREAD_COUNT"
  let threads = maybe 1 read mThreads :: Int
  usersForThreads <- replicateM threads makeRandomUsers
  putStrLn $ "Running nameservice interaction with #threads: " <> show threads
  forever $ forConcurrently_ [0..(threads-1)] $ \i ->
    actionBlock $ usersForThreads !! i
