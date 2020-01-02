module Main where

import           Data.String (fromString)
import           Faker.Name  (name)
import           Interact
import Control.Monad (forever)

main :: IO ()
main = do
  putStrLn "Running nameservice interaction..."
  _ <- faucetAccount user1 1000
  _ <- faucetAccount user2 1000
  forever $ do
    genName <- name
    putStrLn $ "Generated name: " <> genName
    let aName = fromString genName
    _ <- createName user1 aName "no val"
    _ <- buyName user2 aName "some val" 10
    _ <- setName user2 aName "some val (again)"
    _ <- deleteName user2 aName
    putStrLn $ "Loop completed."
