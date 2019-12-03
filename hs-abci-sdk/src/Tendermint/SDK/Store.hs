{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

module Tendermint.SDK.Store
  ( RawStore(..)
  , RawKey(..)
  , IsKey(..)
  , StoreKey(..)
  , ConnectionScope(..)
  , get
  , put
  , delete
  , prove
  , withTransaction
  , withSandbox
  , beginTransaction
  , commitTransaction
  ) where

import           Control.Lens            (Iso', (^.))
import qualified Data.ByteString         as BS
import           Data.Proxy
import           Data.String.Conversions (cs)
import           Polysemy                (Member, Members, Sem, makeSem)
import           Polysemy.Error          (Error, catch, throw)
import           Polysemy.Resource       (Resource, finally, onException)
import           Polysemy.Tagged         (Tagged, tag)
import           Tendermint.SDK.Codec    (HasCodec (..))
import           Tendermint.SDK.Errors   (AppError, SDKError (ParseError),
                                          throwSDKError)

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

data ConnectionScope = Query | Mempool | Consensus

put
  :: forall (c :: ConnectionScope) k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Member (Tagged c RawStore) r
  => StoreKey ns
  -> k
  -> Value k ns
  -> Sem r ()
put sk k a =
  let key = prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey
      val = encode a
  in tag $ rawStorePut sk key val

get
  :: forall (c :: ConnectionScope) k r ns.
     IsKey k ns
  => HasCodec (Value k ns)
  => Members [Tagged c RawStore, Error AppError] r
  => StoreKey ns
  -> k
  -> Sem r (Maybe (Value k ns))
get sk k = do
  let key = prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey
  mRes <- tag $ rawStoreGet sk key
  case mRes of
    Nothing -> pure Nothing
    Just raw -> case decode raw of
      Left e  -> throwSDKError (ParseError $ "Impossible codec error "  <> cs e)
      Right a -> pure $ Just a

delete
  :: forall (c :: ConnectionScope) k ns r.
     IsKey k ns
  => Member (Tagged c RawStore) r
  => StoreKey ns
  -> k
  -> Sem r ()
delete sk k = tag $ rawStoreDelete sk $
  prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey

prove
  :: forall (c :: ConnectionScope) k ns r.
     IsKey k ns
  => Member (Tagged c RawStore) r
  => StoreKey ns
  -> k
  -> Sem r (Maybe BS.ByteString)
prove sk k = tag $ rawStoreProve sk $
  prefixWith (Proxy @k) (Proxy @ns) <> k ^. rawKey


beginTransaction
  :: Member (Tagged 'Consensus RawStore) r
  => Sem r ()
beginTransaction = tag rawStoreBeginTransaction

commitTransaction
  :: Member (Tagged 'Consensus RawStore) r
  => Sem r ()
commitTransaction = tag rawStoreCommit

withTransaction
  :: forall r a.
     Members [Tagged 'Consensus RawStore, Resource, Error AppError] r
  => Sem r a
  -> Sem r a
withTransaction m =
   let tryTx = m `catch` (\e -> tag rawStoreRollback *> throw e)
   in do
      tag rawStoreBeginTransaction
      onException (tryTx <* tag rawStoreCommit) (tag rawStoreRollback)

withSandbox
  :: forall r a.
     Members [Tagged 'Mempool RawStore, Resource, Error AppError] r
  => Sem r a
  -> Sem r a
withSandbox m =
   let tryTx = m `catch` (\e -> tag rawStoreRollback *> throw e)
   in do
      tag rawStoreBeginTransaction
      finally (tryTx <* tag rawStoreRollback) (tag rawStoreRollback)
