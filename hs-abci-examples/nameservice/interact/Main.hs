module Main where

import           Control.Concurrent (forkIO)
import           Control.Monad      (forever)
import           Data.Foldable      (for_)
import           Data.Maybe         (maybe)
import           Interact
import           System.Environment (lookupEnv)
import           Text.Read          (read)

main :: IO ()
main = do
  mThreads <- lookupEnv "TX_COUNT"
  let threads = maybe 1 read mThreads :: Int
  putStrLn $ "Running nameservice interaction with #threads: " <> show threads
  faucetAccount user1 10000
  faucetAccount user2 10000
  for_ [1..threads] $ \_ -> forkIO . forever $ actionBlock (user1, user2)
