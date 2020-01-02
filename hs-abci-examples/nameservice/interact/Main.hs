module Main where

import           Data.String (fromString)
import           Faker.Name  (name)
import           Interact
import Control.Monad (forever)

main :: IO ()
main = do
  putStrLn "Running nameservice interaction..."
  faucetAccount user1 1000
  faucetAccount user2 1000
  forever $ do
    genName <- name
    putStrLn $ "Generated name: " <> genName
    let aName = fromString genName
    createName user1 aName "no val"
    buyName user2 aName "some val" 10
    setName user2 aName "some val (again)"
    deleteName user2 aName
