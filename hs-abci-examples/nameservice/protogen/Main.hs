module Main where

import Protogen (whoisProtoFile, messagesProtoFile)  

main :: IO ()
main = do
  putStrLn messagesProtoFile
  putStrLn whoisProtoFile
