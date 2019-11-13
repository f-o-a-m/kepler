{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Store
  ( MultiStore(..)
  , Store
  , eval
  , StoreKey(..)
  , IsRawKey(..)
  , IsKey(..)
  , get
  , put
  , delete
  , prove
  ) where

import           Control.Lens            (Iso', (^.))
import qualified Data.ByteString         as BS
import           Data.Proxy
import           Data.String.Conversions (cs)
import           GHC.TypeLits            (KnownSymbol, Symbol, symbolVal)
import           Polysemy                (Member, Sem, interpret, makeSem)
import           Tendermint.SDK.Codec    (HasCodec (..))

newtype StoreKey = StoreKey BS.ByteString

data MultiStore m a where
  MultiStorePut   :: StoreKey -> BS.ByteString -> BS.ByteString -> MultiStore m ()
  MultiStoreGet   :: StoreKey -> BS.ByteString -> MultiStore m (Maybe BS.ByteString)
  MultiStoreDelete :: StoreKey -> BS.ByteString -> MultiStore m ()
  MultiStoreProve :: StoreKey -> BS.ByteString -> MultiStore m (Maybe BS.ByteString)

makeSem ''MultiStore

data Store (storeKey :: Symbol) m a where
  StorePut   :: BS.ByteString -> BS.ByteString -> Store storeKey m ()
  StoreGet   :: BS.ByteString -> Store storeKey m (Maybe BS.ByteString)
  StoreDelete :: BS.ByteString -> Store storeKey m ()
  StoreProve :: BS.ByteString -> Store storeKey m (Maybe BS.ByteString)

makeSem ''Store

eval
  :: forall r storeKey.
     KnownSymbol storeKey
  => Member MultiStore r
  => (forall a. Sem (Store storeKey ': r) a -> Sem r a)
eval = interpret (\case
  StorePut k v -> multiStorePut (StoreKey . cs . symbolVal $ Proxy @storeKey) k v
  StoreGet k -> multiStoreGet (StoreKey . cs . symbolVal $ Proxy @storeKey) k
  StoreDelete k -> multiStoreDelete (StoreKey . cs . symbolVal $ Proxy @storeKey) k
  StoreProve k -> multiStoreProve (StoreKey . cs . symbolVal $ Proxy @storeKey) k
  )

--------------------------------------------------------------------------------

class IsRawKey k where
  rawKey :: Iso' k BS.ByteString

class IsRawKey k => IsKey storeKey k where
  type Value storeKey k = a | a -> storeKey k


put
  :: forall k storeKey r.
     IsKey storeKey k
  => HasCodec (Value storeKey k)
  => Member (Store storeKey) r
  => k
  -> Value storeKey k
  -> Sem r ()
put k a =
  let key = k ^. rawKey
      val = encode a
  in storePut key val

get
  :: forall k storeKey r.
     IsKey storeKey k
  => HasCodec (Value storeKey k)
  => Member (Store storeKey) r
  => k
  -> Sem r (Maybe (Value storeKey k))
get k = do
  let key = k ^. rawKey
  mRes <- storeGet key
  pure $ case mRes of
    Nothing -> Nothing
    Just raw -> case decode raw of
      Left e  -> error $ "Impossible codec error "  <> e
      Right a -> Just a

delete
  :: forall storeKey k r.
     IsKey storeKey k
  => Member (Store storeKey) r
  => k
  -> Sem r ()
delete k = storeDelete (k ^. rawKey)

prove
  :: forall storeKey k r.
     IsKey storeKey k
  => Member (Store storeKey) r
  => k
  -> Sem r (Maybe BS.ByteString)
prove k = storeProve (k ^. rawKey)
