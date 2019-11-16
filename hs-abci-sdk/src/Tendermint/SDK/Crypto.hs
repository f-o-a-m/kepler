module Tendermint.SDK.Crypto where

import           Crypto.Hash            (hashWith)
import           Crypto.Hash.Algorithms (Keccak_256 (..))
import           Data.ByteArray         (convert)
import           Data.ByteString        (ByteString)

keccak256 :: ByteString -> ByteString
keccak256 = convert . hashWith Keccak_256
