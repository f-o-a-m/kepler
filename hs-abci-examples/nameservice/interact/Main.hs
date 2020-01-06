module Main where

import           Control.Monad      (forever)
import           Data.Maybe         (maybe)
import           Interact
import           System.Environment (lookupEnv)
import           Text.Read          (read)

main :: IO ()
main = do
  mConc <- lookupEnv "TX_COUNT"
  let conc = maybe 1 read mConc
  putStrLn $ "Running nameservice interaction w/ TX_COUNT: " <> show conc
  faucetAccount user1 10000
  faucetAccount user2 10000
  forever $ do
    names <- genNames conc
    vals <- genVals conc
    buyVals <- genVals conc
    let buyAmts = replicate conc 10
    setVals <- genVals conc
    createNames user1 (zip names vals)
    buyNames user2 (zip3 names buyVals buyAmts)
    setNames user2 (zip names setVals)
    deleteNames user2 names
