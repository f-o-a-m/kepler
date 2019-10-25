{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Store
  ( RawStore(..)
  , HasKey(..)
  , Queryable(..)
  , Root(..)
  , get
  , put
  , prove
  , root
  ) where

import           Control.Lens                (Iso', (^.))
import qualified Data.ByteString             as BS
import           GHC.TypeLits                (Symbol)
import           Polysemy
import           Tendermint.SDK.Codec

newtype Root = Root BS.ByteString

data RawStore m a where
  RawStorePut   :: BS.ByteString -> BS.ByteString -> RawStore m ()
  RawStoreGet   :: Root -> BS.ByteString -> RawStore m (Maybe BS.ByteString)
  RawStoreProve :: Root -> BS.ByteString -> RawStore m (Maybe BS.ByteString)
  RawStoreRoot  :: RawStore m Root

makeSem ''RawStore

class HasCodec a => HasKey a where
    type Key a = k | k -> a
    rawKey :: Iso' (Key a) BS.ByteString

class HasKey a => Queryable a where
  type Name a :: Symbol

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

prove
  :: HasKey a
  => Member RawStore r
  => Root
  -> Key a
  -> Sem r (Maybe BS.ByteString)
prove r k = rawStoreProve r (k ^. rawKey)
