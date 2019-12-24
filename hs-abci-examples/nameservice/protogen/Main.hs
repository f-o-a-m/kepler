module Main where

import           Protogen (messagesProtoFile, whoisProtoFile)

main :: IO ()
main = do
  putStrLn messagesProtoFile
  putStrLn whoisProtoFile
