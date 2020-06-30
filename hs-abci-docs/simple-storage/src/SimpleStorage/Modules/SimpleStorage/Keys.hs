module SimpleStorage.Modules.SimpleStorage.Keys
  ( countKey
  ) where

import           Crypto.Hash             (SHA256 (..), hashWith)
import           Data.ByteArray          (convert)
import           Data.ByteString         (ByteString)
import           Data.String.Conversions (cs)


countKey :: ByteString
countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)
