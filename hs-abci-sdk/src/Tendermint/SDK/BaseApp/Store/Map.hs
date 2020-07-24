{-# LANGUAGE NoImplicitPrelude #-}

module Tendermint.SDK.BaseApp.Store.Map
  ( Map
  , makeMap
  , makeFullStoreKey
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
import Data.Kind (Type)

data Map (k :: Type) (v :: Type) = Map
  { mapStore :: S.Store (Map k v)
  }

instance S.RawKey k => S.IsKey k (Map k v) where
  type Value k (Map k v) = v

makeMap
  :: S.IsKey key ns
  => S.Value key ns ~ Map k v
  => key
  -> S.Store ns
  -> S.Value key ns
makeMap key store =
  let skr :: S.KeyRoot (Map k v)
      skr = S.KeyRoot $ key ^. S.rawKey
  in Map $ S.nestStore store (S.makeStore skr)

makeFullStoreKey
  :: S.RawKey k
  => Map k v
  -> k
  -> S.StoreKey
makeFullStoreKey Map{..} =
  S.makeStoreKey mapStore

insert
  :: Member S.WriteStore r
  => S.RawKey k
  => HasCodec v
  => k
  -> v
  -> Map k v
  -> Sem r ()
insert k v Map{..} =
  S.put mapStore k v

lookup
  :: Members [Error AppError, S.ReadStore] r
  => S.RawKey k
  => HasCodec v
  => k
  -> Map k v
  -> Sem r (Maybe v)
lookup k Map{..} =
  S.get mapStore k

delete
  :: Member S.WriteStore r
  => S.RawKey k
  => k
  -> Map k v
  -> Sem r ()
delete k Map{..} =
  S.delete mapStore k

update
  :: Members [Error AppError, S.ReadStore, S.WriteStore] r
  => S.RawKey k
  => HasCodec v
  => (v -> Maybe v)
  -> k
  -> Map k v
  -> Sem r ()
update f k store = do
  mv <- lookup k store
  case mv of
    Nothing -> pure ()
    Just v  -> maybe (delete k store) (\a -> insert k a store) (f v)
