module Tendermint.SDK.Store
  ( RawStore(..)
  , Store(..)
  , Codecs(..)
  , HasRootKey(..)
  , StoreKey(..)
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

class HasRootKey a where
    type RootKey a :: Symbol
    rootKey :: Proxy a -> BS.ByteString
    default rootKey :: KnownSymbol (RootKey a) => Proxy a -> BS.ByteString
    rootKey _ = cs $ symbolVal (Proxy :: Proxy (RootKey a))

class HasRootKey a => StoreKey a k | k -> a where
    makeRawStoreKey :: Proxy a -> k -> BS.ByteString

data Store contents hash m = Store
  { storeRawStore :: RawStore m hash
  , storeCodecs :: Codecs contents
  }

mkKey
  :: forall a k.
     StoreKey a k
  => k
  -> BS.ByteString
mkKey k = 
  let rk = rootKey (Proxy :: Proxy a)
      key = makeRawStoreKey (Proxy :: Proxy a) k
  in rk <> key

root
  :: Store contents hash m
  -> m hash
root Store{storeRawStore} = rawStoreRoot storeRawStore

put
  :: forall a k contents hash m.
     HasCodec a contents
  => StoreKey a k
  => k
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
  :: forall a k contents hash m.
     HasCodec a contents
  => StoreKey a k
  => Monad m
  => k
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
