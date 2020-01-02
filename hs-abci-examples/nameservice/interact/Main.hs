module Main where

import           Interact

main :: IO ()
main = do
  putStrLn "Running nameservice interaction..."
  _ <- faucetAccount user1 1000
  _ <- faucetAccount user2 1000
  _ <- createName user1 "anyName" "no val"
  _ <- buyName user2 "anyName" "some val" 10
  _ <- setName user2 "anyName" "some val (again)"
  _ <- deleteName user2 "anyName"
  putStrLn "Nameservice interaction complete."
