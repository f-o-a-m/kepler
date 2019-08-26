module Tendermint.SDK.Codec where

import qualified Data.ByteString as BS

data Codec a = Codec
  { codecEncode :: a -> BS.ByteString
  , codecDecode :: BS.ByteString -> Either String a
  }

data Codecs :: [*] -> * where
    NilCodecs :: Codecs '[]
    (:~) :: Codec a -> Codecs as -> Codecs (a : as)

class HasCodec c cs where
    getCodec :: Codecs cs -> Codec c

instance {-# OVERLAPPING #-} HasCodec c (c : cs)  where
    getCodec (aCodec :~ _) = aCodec

instance {-# OVERLAPPABLE #-} HasCodec c cs => HasCodec c (c' : cs) where
    getCodec (_ :~ rest) = getCodec rest
