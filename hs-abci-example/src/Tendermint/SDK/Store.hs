module Tendermint.SDK.Store
  ( RawStore(..)
  , Store(..)
  , HasKey(..)
  , Queryable(..)
  , Root(..)
  , get
  , put
  , prove
  , root
  ) where

import           Control.Lens         (Iso', (^.))
import qualified Data.ByteString      as BS
import           Tendermint.SDK.Codec
import           GHC.TypeLits         (Symbol)

newtype Root = Root BS.ByteString

data RawStore m = RawStore
  { rawStorePut  :: BS.ByteString -> BS.ByteString -> m ()
  , rawStoreGet  :: Root -> BS.ByteString -> m (Maybe BS.ByteString)
  , rawStoreProve :: Root -> BS.ByteString -> m (Maybe BS.ByteString)
  , rawStoreRoot :: m Root
  }

class HasKey a where
    type Key a = k | k -> a
    rawKey :: Iso' (Key a) BS.ByteString

class HasKey a => Queryable a where
  type Name a :: Symbol
  
data Store contents m = Store
  { storeRawStore :: RawStore m
  }

root
  :: Store contents m
  -> m Root
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
  => Root
  -> Key a
  -> Store contents m
  -> m (Maybe a)
get index k Store{storeRawStore} = do
    let RawStore {rawStoreGet} = storeRawStore
        key = k ^. rawKey
    mRes <- rawStoreGet index key
    pure $ case mRes of
        Nothing -> Nothing
        Just raw -> case decode raw of
          Left e  -> error $ "Impossible codec error "  <> e
          Right a -> Just a

prove 
  :: HasKey a
  => Monad m
  => Root
  -> Key a
  -> Store contents m
  -> m (Maybe BS.ByteString)
prove _ _ _ = pure Nothing