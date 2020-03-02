{-# LANGUAGE NoImplicitPrelude #-}

module Tendermint.SDK.BaseApp.Store.LinkedList
  ( LinkedList
  , makeLinkedList
  , append
  , delete
  , deleteWhen
  , foldl
  , toList
  , length
  , elemIndex
  , (!!)
  ) where

import           Control.Lens                          (from, iso, to, view,
                                                        (^.))
import           Control.Monad                         (when)
import qualified Data.ByteArray.HexString              as Hex
import           Data.String.Conversions               (cs)
import           Data.Word                             (Word64)
import           Polysemy                              (Members, Sem)
import           Polysemy.Error                        (Error)
import           Prelude                               hiding ((!!), foldl, length)
import           Tendermint.SDK.BaseApp.Errors         (AppError,
                                                        SDKError (InternalError),
                                                        throwSDKError)
import qualified Tendermint.SDK.BaseApp.Store.Map      as M
import qualified Tendermint.SDK.BaseApp.Store.RawStore as S
import           Tendermint.SDK.Codec                  (HasCodec (..))

data LinkedList (a :: *) = LinkedList
  { linkedListStore :: S.Store (LinkedList a)
  }

newtype Idx = Idx {unIdx :: Word64} deriving (Eq, Show, Ord, Num)

instance S.RawKey Idx where
    rawKey = iso (\(Idx ma) -> ma ^. S.rawKey)
                (\bs -> bs ^. from S.rawKey . to Idx)

instance HasCodec Idx where
    encode = view S.rawKey
    decode = Right . view (from S.rawKey)

data IdxKey = IdxKey

instance S.RawKey IdxKey where
  rawKey =
    let k = Hex.toBytes "0x00"
    in iso (const k)
         (\bs -> if bs == k
                   then IdxKey
                   else error "Error parsing IdxKey"
         )

instance S.IsKey IdxKey (LinkedList a) where
  type Value IdxKey (LinkedList a) = M.StoreMap Idx Idx

data ValueKey = ValueKey

instance S.RawKey ValueKey where
  rawKey =
    let k = Hex.toBytes "0x01"
    in iso (const k)
         (\bs -> if bs == k
                   then ValueKey
                   else error "Error parsing ValueKey"
         )

instance S.IsKey ValueKey (LinkedList a) where
  type Value ValueKey (LinkedList a) = M.StoreMap Idx a

makeLinkedList
  :: S.IsKey key ns
  => S.Value key ns ~ LinkedList a
  => key
  -> S.Store ns
  -> S.Value key ns
makeLinkedList key store =
  LinkedList $ S.nestStore store $
    S.makeStore . S.KeyRoot $ key ^. S.rawKey

getIdxMap
  :: LinkedList a
  -> M.StoreMap Idx Idx
getIdxMap LinkedList{..} =
  M.makeStoreMap IdxKey linkedListStore

getValueMap
  :: LinkedList a
  -> M.StoreMap Idx a
getValueMap LinkedList{..} =
  M.makeStoreMap ValueKey linkedListStore

data HeadKey = HeadKey

instance S.RawKey HeadKey where
  rawKey =
    let k = Hex.toBytes "0x02"
    in iso (const k)
         (\bs -> if bs == k
                   then HeadKey
                   else error "Error parsing HeadKey"
         )

instance S.IsKey HeadKey (LinkedList a) where
  type Value HeadKey (LinkedList a) = Idx

assertLookup
  :: Members [S.ReadStore, Error AppError] r
  => S.RawKey k
  => HasCodec v
  => k
  -> M.StoreMap k v
  -> Sem r v
assertLookup k m = do
  mRes <- M.lookup k m
  case mRes of
    Nothing -> throwSDKError $
      InternalError $ "Database integrity failed, lookup miss: " <> cs (k ^. S.rawKey)
    Just res -> pure res

append
  :: Members [Error AppError, S.ReadStore, S.WriteStore] r
  => HasCodec a
  => a
  -> LinkedList a
  -> Sem r ()
append a l@LinkedList{..} = do
  mhd <- S.get linkedListStore HeadKey
  let valueMap = getValueMap l
  case mhd of
    Nothing -> do
      S.put linkedListStore HeadKey 0
      M.insert 0 a valueMap
    Just hd -> do
      let hd' = hd + 1
          idxMap = getIdxMap l
      M.insert  hd' hd idxMap
      M.insert hd' a valueMap
      S.put linkedListStore HeadKey hd'

-- | Delete the first occurence in the list.
delete
  :: Members [Error AppError, S.ReadStore, S.WriteStore] r
  => HasCodec a
  => Eq a
  => a
  -> LinkedList a
  -> Sem r ()
delete a l@LinkedList{..} = do
  mhd <- S.get linkedListStore HeadKey
  case mhd of
    -- the list looks like []
    Nothing -> pure ()
    -- the list looks like (? : as)
    Just hd -> do
      let valueMap = getValueMap l
          idxMap = getIdxMap l
      a' <- assertLookup hd valueMap
      mNext <- M.lookup hd idxMap
      if a'== a
        -- the list looks like (a : as)
        then deleteHead l
        -- the list looks like (b : as)
        else delete' hd mNext
    where
      delete' prevIdx mcurrIdx =
        case mcurrIdx of
          Nothing -> pure ()
          Just currIdx -> do
            let valueMap = getValueMap l
                idxMap = getIdxMap l
            a' <- assertLookup currIdx valueMap
            mNext <- M.lookup currIdx idxMap
            if a == a'
              then snipNode prevIdx currIdx l
              else delete' currIdx mNext

-- | Delete an element whenever the predicate evaluates to 'True'
deleteWhen
  :: Members [Error AppError, S.ReadStore, S.WriteStore] r
  => HasCodec a
  => (a -> Bool)
  -> LinkedList a
  -> Sem r ()
deleteWhen p l@LinkedList{..} = do
  mhd <- S.get linkedListStore HeadKey
  case mhd of
    Nothing -> pure ()
    Just hd -> do
      let valueMap = getValueMap l
          idxMap = getIdxMap l
      a <- assertLookup hd valueMap
      mNext <- M.lookup hd idxMap
      when (p a) $
        deleteHead l
      deleteWhen' hd mNext
  where
    deleteWhen' prevIdx mcurrIdx =
      case mcurrIdx of
        Nothing -> pure ()
        Just currIdx -> do
          let valueMap = getValueMap l
              idxMap = getIdxMap l
          a <- assertLookup currIdx valueMap
          mNext <- M.lookup currIdx idxMap
          when (p a) $
            snipNode prevIdx currIdx l
          deleteWhen' currIdx mNext

foldl
  :: Members [Error AppError, S.ReadStore] r
  => HasCodec a
  => (b -> a -> b)
  -> b
  -> LinkedList a
  -> Sem r b
foldl f b l@LinkedList{..} = do
  mhd <- S.get linkedListStore HeadKey
  foldl' mhd b
  where
    foldl' mcurrentHead acc =
      case mcurrentHead of
        Nothing -> pure acc
        Just hd -> do
          let valMap = getValueMap l
              idxMap = getIdxMap l
          a <- assertLookup hd valMap
          mNext <- M.lookup hd idxMap
          foldl' mNext $! f acc a

-- | View the 'LinkedList' as a 'List'.
toList
  :: Members [Error AppError, S.ReadStore] r
  => HasCodec a
  => LinkedList a
  -> Sem r [a]
toList = foldl (flip (:)) []

length
  :: Members [Error AppError, S.ReadStore] r
  => HasCodec a
  => LinkedList a
  -> Sem r Word64
length = foldl (\n _ -> n + 1) 0

elemIndex
  :: Members [Error AppError, S.ReadStore] r
  => HasCodec a
  => Eq a
  => a
  -> LinkedList a
  -> Sem r (Maybe Word64)
elemIndex a l@LinkedList{..} = do
  mhd <- S.get linkedListStore HeadKey
  elemIndex' 0 mhd
  where
    elemIndex' i mcurrentHead =
      case mcurrentHead of
        Nothing -> pure Nothing
        Just hd -> do
          let valMap = getValueMap l
          a' <- assertLookup hd valMap
          if a == a'
            then pure $ Just $ unIdx i
            else do
              let idxMap = getIdxMap l
              mNext <- M.lookup hd idxMap
              elemIndex' (i + 1) mNext

(!!)
  :: Members [Error AppError, S.ReadStore] r
  => HasCodec a
  => LinkedList a
  -> Word64
  -> Sem r (Maybe a)
l@LinkedList{..} !! idx = do
  mhd <- S.get linkedListStore HeadKey
  getAtIndex 0 mhd
  where
    getAtIndex currPos mhd =
      case mhd of
        Nothing -> pure Nothing
        Just hd ->
          if idx == currPos
            then
              let valMap = getValueMap l
              in Just <$> assertLookup hd valMap
            else do
              let idxMap = getIdxMap l
              mNext <- M.lookup hd idxMap
              getAtIndex (currPos + 1) mNext

infixl 9  !!

--------------------------------------------------------------------------------

snipNode
  :: Members [Error AppError, S.ReadStore, S.WriteStore] r
  => Idx
  -- ^ previous index
  -> Idx
  -- ^ current index (node to delete)
  -> LinkedList a
  -> Sem r ()
snipNode prevIdx currIdx l = do
  let valueMap = getValueMap l
      idxMap = getIdxMap l
  -- remove the value that currIdx points to
  M.delete currIdx valueMap
  mNext <- M.lookup currIdx idxMap
  case mNext of
    -- (n) - (a) ~> (n)
    Nothing -> M.delete prevIdx idxMap
    -- (n) - (a) - (n') ~> (n) - (n')
    Just next -> do
      M.delete currIdx idxMap
      M.insert prevIdx next idxMap

deleteHead
  :: Members [Error AppError, S.ReadStore, S.WriteStore] r
  => LinkedList a
  -> Sem r ()
deleteHead l@LinkedList{..} = do
  mhd <- S.get linkedListStore HeadKey
  case mhd of
    Nothing -> pure ()
    Just hd -> do
      let valueMap = getValueMap l
          idxMap = getIdxMap l
      M.delete hd valueMap
      mNext <- M.lookup hd idxMap
      case mNext of
        Nothing -> S.delete linkedListStore HeadKey
        Just next -> do
          M.delete hd idxMap
          S.put linkedListStore HeadKey next
