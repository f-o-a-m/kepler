module Tendermint.SDK.Store
  ( RawStore(..)
  , Store(..)
  , HasKey(..)
  , get
  , put
  , root
  ) where

import           Control.Lens         (Iso', (^.))
import qualified Data.ByteString      as BS
import           Tendermint.SDK.Codec

data RawStore m = RawStore
  { rawStorePut  :: BS.ByteString -> BS.ByteString -> m ()
  , rawStoreGet  :: BS.ByteString -> m (Maybe BS.ByteString)
  , rawStoreRoot :: m BS.ByteString
  }

class HasKey a where
    type Key a = k | k -> a
    rawKey :: Iso' (Key a) BS.ByteString

data Store contents m = Store
  { storeRawStore :: RawStore m
  }

root
  :: Store contents m
  -> m BS.ByteString
root Store{storeRawStore} = rawStoreRoot storeRawStore

put
  :: forall a contents m.
     ContainsCodec a contents
  => HasKey a
  => Key a
  -> a
  -> Store contents m
  -> m ()
put k a Store{storeRawStore} = do
    let RawStore {rawStorePut} = storeRawStore
        key = k ^. rawKey
        val = encode a
    rawStorePut key val

get
  :: forall a contents m.
     ContainsCodec a contents
  => HasKey a
  => Monad m
  => Key a
  -> Store contents m
  -> m (Maybe a)
get k Store{storeRawStore} = do
    let RawStore {rawStoreGet} = storeRawStore
        key = k ^. rawKey
    mRes <- rawStoreGet key
    pure $ case mRes of
        Nothing -> Nothing
        Just raw -> case decode raw of
          Left e  -> error $ "Impossible codec error "  <> e
          Right a -> Just a
{-

"name" :> Leaf k

class CreateRoutes s rs where


-}
