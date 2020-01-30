module Tendermint.SDK.BaseApp.Transaction.Cache
  ( Cache
  , emptyCache
  , writeCache
  , Deleted(..)
  , put
  , get
  , delete
  ) where

import           Data.ByteString                       (ByteString)
import           Data.Map.Strict                       (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Set                              (Set)
import qualified Data.Set                              as Set
import           Polysemy                              (Member, Sem)
import           Tendermint.SDK.BaseApp.Store.RawStore (RawStoreKey, WriteStore,
                                                        storeDelete, storePut)

data Cache = Cache
  { keysToDelete :: Set RawStoreKey
  , stateCache   :: Map RawStoreKey ByteString
  }

emptyCache :: Cache
emptyCache = Cache Set.empty Map.empty

put
  :: RawStoreKey
  -> ByteString
  -> Cache
  -> Cache
put k v Cache{..} =
  let keysToDelete' = Set.delete k keysToDelete
      stateCache' = Map.insert k v stateCache
  in Cache keysToDelete' stateCache'

data Deleted = Deleted

get
  :: RawStoreKey
  -> Cache
  -> Either Deleted (Maybe ByteString)
get k Cache{..} =
  if k `Set.member` keysToDelete
    then Left Deleted
    else Right $ Map.lookup k stateCache

delete
  :: RawStoreKey
  -> Cache
  -> Cache
delete k Cache{..} =
  let keysToDelete' = Set.insert k keysToDelete
      stateCache' = Map.delete k stateCache
  in Cache keysToDelete' stateCache'

writeCache
  :: Member WriteStore r
  => Cache
  -> Sem r ()
writeCache Cache{..} = do
  mapM_ (uncurry storePut) (Map.toList stateCache)
  mapM_ storeDelete (Set.toList keysToDelete)
