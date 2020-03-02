{-# LANGUAGE NoImplicitPrelude #-}

module Tendermint.SDK.BaseApp.Store.Map
  ( StoreMap
  , makeStoreMap
  , insert
  , lookup
  , delete
  , update
  ) where

import           Control.Lens                          ((^.))
import           Polysemy                              (Member, Members, Sem)
import           Polysemy.Error                        (Error)
import           Prelude                               hiding (lookup)
import           Tendermint.SDK.BaseApp.Errors         (AppError)
import qualified Tendermint.SDK.BaseApp.Store.RawStore as S
import           Tendermint.SDK.Codec                  (HasCodec (..))

data StoreMap (k :: *) (v :: *) = StoreMap
  { storeMapStore :: S.Store (StoreMap k v)
  }

instance S.RawKey k => S.IsKey k (StoreMap k v) where
  type Value k (StoreMap k v) = v

makeStoreMap
  :: S.IsKey key ns
  => S.Value key ns ~ StoreMap k v
  => key
  -> S.Store ns
  -> S.Value key ns
makeStoreMap key store =
  let skr :: S.KeyRoot (StoreMap k v)
      skr = S.KeyRoot $ key ^. S.rawKey
  in StoreMap $ S.nestStore store (S.makeStore skr)

insert
  :: Member S.WriteStore r
  => S.RawKey k
  => HasCodec v
  => k
  -> v
  -> StoreMap k v
  -> Sem r ()
insert k v StoreMap{..} =
  S.put storeMapStore k v

lookup
  :: Members [Error AppError, S.ReadStore] r
  => S.RawKey k
  => HasCodec v
  => k
  -> StoreMap k v
  -> Sem r (Maybe v)
lookup k StoreMap{..} =
  S.get storeMapStore k

delete
  :: Member S.WriteStore r
  => S.RawKey k
  => k
  -> StoreMap k v
  -> Sem r ()
delete k StoreMap{..} =
  S.delete storeMapStore k

update
  :: Members [Error AppError, S.ReadStore, S.WriteStore] r
  => S.RawKey k
  => HasCodec v
  => (v -> Maybe v)
  -> k
  -> StoreMap k v
  -> Sem r ()
update f k store = do
  mv <- lookup k store
  case mv of
    Nothing -> pure ()
    Just v  -> maybe (delete k store) (\a -> insert k a store) (f v)
