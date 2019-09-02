module Tendermint.SDK.Codec where

import qualified Data.ByteString as BS

class HasCodec a where
    encode :: a -> BS.ByteString
    decode :: BS.ByteString -> Either String a

class HasCodec c => ContainsCodec c cs where

instance {-# OVERLAPPING #-} HasCodec c => ContainsCodec c (c : cs)

instance {-# OVERLAPPABLE #-} (HasCodec c, ContainsCodec c cs) => ContainsCodec c (c' : cs)