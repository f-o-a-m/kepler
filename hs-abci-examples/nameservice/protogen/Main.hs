-- @NOTE: ^ possibly the only language extension needed from tutorial
-- Seems to be the only requirement for generating the .proto string

module Main where

import           Protogen (messagesProtoFile, whoisProtoFile)

main :: IO ()
main = do
  putStrLn messagesProtoFile
  putStrLn whoisProtoFile
