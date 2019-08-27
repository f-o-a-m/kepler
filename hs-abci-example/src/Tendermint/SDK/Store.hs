module Tendermint.SDK.Store
  ( RawStore(..)
  , Store(..)
  , Codecs(..)
  , HasKey(..)
  , get
  , put
  , root
  ) where

import qualified Data.ByteString as BS
import Tendermint.SDK.Codec

data RawStore m = RawStore
  { rawStorePut :: BS.ByteString -> BS.ByteString -> m ()
  , rawStoreGet :: BS.ByteString -> m (Maybe BS.ByteString)
  , rawStoreRoot :: m BS.ByteString
  }

class HasKey a where
    type Key a = k | k -> a
    makeRawKey :: Key a -> BS.ByteString

data Store contents m = Store
  { storeRawStore :: RawStore m
  , storeCodecs :: Codecs contents
  }

root
  :: Store contents m
  -> m BS.ByteString
root Store{storeRawStore} = rawStoreRoot storeRawStore

put
  :: forall a contents m.
     HasCodec a contents
  => HasKey a
  => Key a
  -> a
  -> Store contents m
  -> m ()
put k a Store{storeRawStore, storeCodecs} = do
    let codec = getCodec storeCodecs
        RawStore {rawStorePut} = storeRawStore
        key = makeRawKey k
        val = codecEncode codec $ a
    rawStorePut key val

get 
  :: forall a contents m.
     HasCodec a contents
  => HasKey a
  => Monad m
  => Key a
  -> Store contents m
  -> m (Maybe a)
get k Store{storeRawStore, storeCodecs} = do
    let codec = getCodec storeCodecs
        RawStore {rawStoreGet} = storeRawStore
        key = makeRawKey k
    mRes <- rawStoreGet key
    pure $ case mRes of
        Nothing -> Nothing
        Just raw -> case codecDecode codec raw of
          Left e -> error $ "Impossible codec error "  <> e
          Right a -> Just a


{-

"name" :> Leaf k

class CreateRoutes s rs where


-}