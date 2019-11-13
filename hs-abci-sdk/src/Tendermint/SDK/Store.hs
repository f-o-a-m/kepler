{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Store
  ( RawStore(..)
  , RawKey(..)
  , IsKey(..)
  , StoreKey(..)
  , get
  , put
  , delete
  , prove
  ) where

import           Control.Lens         (Iso', (^.))
import qualified Data.ByteString      as BS
import           Polysemy             (Member, Sem, makeSem)
import           Tendermint.SDK.Codec (HasCodec (..))

newtype StoreKey n = StoreKey BS.ByteString

data RawStore m a where
  RawStorePut   :: StoreKey ns -> BS.ByteString -> BS.ByteString -> RawStore m ()
  RawStoreGet   :: StoreKey ns -> BS.ByteString -> RawStore m (Maybe BS.ByteString)
  RawStoreDelete :: StoreKey ns -> BS.ByteString -> RawStore m ()
  RawStoreProve :: StoreKey ns -> BS.ByteString -> RawStore m (Maybe BS.ByteString)

makeSem ''RawStore

class RawKey k where
  rawKey :: Iso' k BS.ByteString

class RawKey k => IsKey k ns where
  type Value k ns = a | a -> ns k

put
  :: forall k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Member RawStore r
  => StoreKey ns
  -> k
  -> Value k ns
  -> Sem r ()
put sk k a =
  let key = k ^. rawKey
      val = encode a
  in rawStorePut sk key val

get
  :: forall k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Member RawStore r
  => StoreKey ns
  -> k
  -> Sem r (Maybe (Value k ns))
get sk k = do
  let key = k ^. rawKey
  mRes <- rawStoreGet sk key
  pure $ case mRes of
    Nothing -> Nothing
    Just raw -> case decode raw of
      Left e  -> error $ "Impossible codec error "  <> e
      Right a -> Just a

delete
  :: IsKey k ns
  => Member RawStore r
  => StoreKey ns
  -> k
  -> Sem r ()
delete sk k = rawStoreDelete sk (k ^. rawKey)

prove
  :: IsKey k ns
  => Member RawStore r
  => StoreKey ns
  -> k
  -> Sem r (Maybe BS.ByteString)
prove sk k = rawStoreProve sk (k ^. rawKey)
