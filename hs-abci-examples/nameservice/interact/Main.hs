module Main where

import           Control.Concurrent.Async (forConcurrently_)
import           Control.Monad            (forever)
import           Data.Maybe               (maybe)
import           Interact
import           System.Environment       (lookupEnv)
import           Text.Read                (read)

main :: IO ()
main = do
  mThreads <- lookupEnv "INTERACT_THREAD_COUNT"
  let threads = maybe 1 read mThreads :: Int
  putStrLn $ "Running nameservice interaction with #threads: " <> show threads
  faucetAccount user1 10000
  faucetAccount user2 10000
  forever $ forConcurrently_ [1..threads] $ const actionBlock
