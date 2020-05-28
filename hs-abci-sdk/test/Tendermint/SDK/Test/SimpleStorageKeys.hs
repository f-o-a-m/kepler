module Tendermint.SDK.Test.SimpleStorageKeys
  ( countKey
  , countsKey
  , paidKey
  ) where


import           Crypto.Hash             (SHA256 (..), hashWith)
import           Data.ByteArray          (convert)
import           Data.ByteString         (ByteString)
import           Data.String.Conversions (cs)


countKey :: ByteString
countKey = hashKey "count"

countsKey :: ByteString
countsKey = hashKey "counts"

paidKey :: ByteString
paidKey = hashKey "paid"


hashKey :: String -> ByteString
hashKey = convert . hashWith SHA256 . cs @String @ByteString
