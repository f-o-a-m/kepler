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
  , rawStoreBeginTransaction
  , rawStoreRollback
  , rawStoreCommit
  ) where

import           Control.Lens            (Iso', (^.))
import qualified Data.ByteString         as BS
import           Data.Proxy
import           Data.String.Conversions (cs)
import           Polysemy                (Member, Sem, makeSem)
import           Tendermint.SDK.Codec    (HasCodec (..))

newtype StoreKey n = StoreKey BS.ByteString

data RawStore m a where
  RawStorePut   :: StoreKey ns -> BS.ByteString -> BS.ByteString -> RawStore m ()
  RawStoreGet   :: StoreKey ns -> BS.ByteString -> RawStore m (Maybe BS.ByteString)
  RawStoreDelete :: StoreKey ns -> BS.ByteString -> RawStore m ()
  RawStoreProve :: StoreKey ns -> BS.ByteString -> RawStore m (Maybe BS.ByteString)
  RawStoreBeginTransaction :: RawStore m ()
  RawStoreRollback :: RawStore m ()
  RawStoreCommit :: RawStore m () 

makeSem ''RawStore

class RawKey k where
  rawKey :: Iso' k BS.ByteString

class RawKey k => IsKey k ns where
  type Value k ns = a | a -> ns k
  prefixWith :: Proxy k -> Proxy ns -> BS.ByteString

  default prefixWith :: Proxy k -> Proxy ns -> BS.ByteString
  prefixWith _ _ = ""

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
  let key = prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey
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
  let key = prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey
  mRes <- rawStoreGet sk key
  pure $ case mRes of
    Nothing -> Nothing
    Just raw -> case decode raw of
      Left e  -> error $ "Impossible codec error "  <> cs e
      Right a -> Just a

delete
  :: forall k ns r.
     IsKey k ns
  => Member RawStore r
  => StoreKey ns
  -> k
  -> Sem r ()
delete sk k = rawStoreDelete sk $
  prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey

prove
  :: forall k ns r.
     IsKey k ns
  => Member RawStore r
  => StoreKey ns
  -> k
  -> Sem r (Maybe BS.ByteString)
prove sk k = rawStoreProve sk $
  prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey
