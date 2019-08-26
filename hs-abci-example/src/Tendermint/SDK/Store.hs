module Tendermint.SDK.Store
  ( RawStore(..)
  , Store(..)
  , Codecs(..)
  , HasStorageKeys(..)
  , get
  , put
  , root
  ) where

import qualified Data.ByteString as BS
import Data.Proxy (Proxy(..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Tendermint.SDK.Codec
import Data.String.Conversions (cs)

data RawStore m hash = RawStore
  { rawStorePut :: BS.ByteString -> BS.ByteString -> m ()
  , rawStoreGet :: BS.ByteString -> m (Maybe BS.ByteString)
  , rawStoreRoot :: m hash
  }

class HasStorageKeys a where
    type RootKey a :: Symbol
    type StoreKey a = k | k -> a

    rootKey :: Proxy a -> BS.ByteString
    default rootKey :: KnownSymbol (RootKey a) => Proxy a -> BS.ByteString
    rootKey _ = cs $ symbolVal (Proxy :: Proxy (RootKey a))

    makeRawStoreKey :: StoreKey a -> BS.ByteString

data Store contents hash m = Store
  { storeRawStore :: RawStore m hash
  , storeCodecs :: Codecs contents
  }

mkKey
  :: forall a.
     HasStorageKeys a
  => StoreKey a
  -> BS.ByteString
mkKey k = 
  let rk = rootKey (Proxy :: Proxy a)
      key = makeRawStoreKey k
  in rk <> key

root
  :: Store contents hash m
  -> m hash
root Store{storeRawStore} = rawStoreRoot storeRawStore

put
  :: forall a contents hash m.
     HasCodec a contents
  => HasStorageKeys a
  => StoreKey a
  -> a
  -> Store contents hash m
  -> m ()
put k a Store{storeRawStore, storeCodecs} = do
    let codec = getCodec storeCodecs
        RawStore {rawStorePut} = storeRawStore
        key = mkKey k
        val = codecEncode codec $ a
    rawStorePut key val

get 
  :: forall a contents hash m.
     HasCodec a contents
  => HasStorageKeys a
  => Monad m
  => StoreKey a
  -> Store contents hash m
  -> m (Maybe a)
get k Store{storeRawStore, storeCodecs} = do
    let codec = getCodec storeCodecs
        RawStore {rawStoreGet} = storeRawStore
        key = mkKey k
    mRes <- rawStoreGet key
    pure $ case mRes of
        Nothing -> Nothing
        Just raw -> case codecDecode codec raw of
          Left e -> error $ "Impossible codec error "  <> e
          Right a -> Just a
