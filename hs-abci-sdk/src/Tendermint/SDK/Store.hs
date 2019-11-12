{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Store
  ( RawStore(..)
  , HasKey(..)
  , Root(..)
  , get
  , put
  , delete
  , prove
  , root
  ) where

import           Control.Lens         (Iso', (^.))
import qualified Data.ByteString      as BS
import           Polysemy             (Member, Sem, makeSem)
import           Tendermint.SDK.Codec (HasCodec (..))

newtype Root = Root BS.ByteString

data RawStore m a where
  RawStorePut   :: BS.ByteString -> BS.ByteString -> RawStore m ()
  RawStoreGet   :: Root -> BS.ByteString -> RawStore m (Maybe BS.ByteString)
  RawStoreDelete :: Root -> BS.ByteString -> RawStore m ()
  RawStoreProve :: Root -> BS.ByteString -> RawStore m (Maybe BS.ByteString)
  RawStoreRoot  :: RawStore m Root

makeSem ''RawStore

class HasCodec a => HasKey a where
    type Key a = k | k -> a
    rawKey :: Iso' (Key a) BS.ByteString

root
  :: Member RawStore r
  => Sem r Root
root = rawStoreRoot

put
  :: forall a r.
     HasKey a
  => Member RawStore r
  => Key a
  -> a
  -> Sem r ()
put k a =
  let key = k ^. rawKey
      val = encode a
  in rawStorePut key val

get
  :: forall a r.
     HasKey a
  => Member RawStore r
  => Root
  -> Key a
  -> Sem r (Maybe a)
get index k = do
  let key = k ^. rawKey
  mRes <- rawStoreGet index key
  pure $ case mRes of
    Nothing -> Nothing
    Just raw -> case decode raw of
      Left e  -> error $ "Impossible codec error "  <> e
      Right a -> Just a

delete
  :: HasKey a
  => Member RawStore r
  => Root
  -> Key a
  -> Sem r ()
delete r k = rawStoreDelete r (k ^. rawKey)

prove
  :: HasKey a
  => Member RawStore r
  => Root
  -> Key a
  -> Sem r (Maybe BS.ByteString)
prove r k = rawStoreProve r (k ^. rawKey)
