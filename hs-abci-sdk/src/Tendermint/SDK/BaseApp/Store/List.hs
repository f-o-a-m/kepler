{-# LANGUAGE NoImplicitPrelude #-}

module Tendermint.SDK.BaseApp.Store.List
  ( StoreList
  , makeStoreList
  , append
  , modifyAtIndex
  , deleteWhen
  , (!!)
  , elemIndex
  , toList
  ) where

import Control.Lens ((^.))
import qualified Data.ByteArray.HexString      as Hex
import qualified Data.ByteString               as BS
import           Data.String.Conversions       (cs)
import           Data.Word                     (Word64)
import           Polysemy
import           Polysemy.Error                (Error)
import           Prelude                       hiding (foldl, length, (!!))
import qualified Prelude                       as P (length)
import           Tendermint.SDK.BaseApp.Errors (AppError, SDKError (InternalError, ParseError),
                                                throwSDKError)
import           Tendermint.SDK.BaseApp.Store  (RawStoreKey (..), ReadStore, rawKey,
                                                WriteStore, storeDelete, IsKey(..),
                                                storeGet, storePut)
import           Tendermint.SDK.Codec          (HasCodec (..))
import qualified Data.Text as T



-- | A 'StoreList a' is an appendable list whose elements can be accessed
-- | by their index. You can also delete from the list, in which case accessing
-- | that index will result in a `Nothing`.
data StoreList a = StoreList (Word64 -> RawStoreKey)

newtype Idx = Idx Word64 deriving (Eq, Show, Ord, Num)

instance RawKey Idx where
  rawKey = lens elementKey unElementKey

instance IsKey Idx (StoreList a) where
  type Value Idx (StoreList a) = a

-- | Smart constuctor to make sure we're making a 'StoreList' from
-- | the appropriate key type.
makeStoreList
  :: Member WriteStore r
  => IsKey k ns
  => Value k ns ~ StoreList a
  => k
  -> Store ns
  -> Value k ns
makeStoreList k store = do
  let rsk = k ^. rawKey
      store' = store { path = path store ++ [rsk] }
  in StoreList $ \i -> 

-- | Add an item to the end of the list.
append
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => a
  -> StoreList a
  -> Sem r ()
append a as = do
  n <- length as
  writeAt (Idx n) (encode a) as

-- | Access an item directly by its index.
(!!)
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => StoreList a
  -> Idx
  -> Sem r (Maybe a)
as@StoreList{..} !! n = do
  len <- length as
  if len - 1 < n
    then pure Nothing
    else do
      let k = RawStoreKey
            { rsStoreKey = storeListKey
            , rsKey = elementKey n
            }
      mRes <- storeGet k
      case mRes of
        Nothing -> pure Nothing
        Just res -> case decode res of
          Right a -> pure (Just a)
          Left e -> throwSDKError (ParseError $ "Impossible codec error: "  <> cs e)

infixl 9  !!

-- | Modify a list at a particular index, return the
-- | updated value if the element was found.
modifyAtIndex
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => Idx
  -> (a -> a)
  -> StoreList a
  -> Sem r (Maybe a)
modifyAtIndex i f as = do
  mRes <- as !! i
  case mRes of
    Nothing -> pure Nothing
    Just res -> do
      let a' = f res 
      writeAt i (encode $ a') as
      pure (Just a')
    
-- | Delete when the predicate evaluates to true.
deleteWhen
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => (a -> Bool)
  -> StoreList a
  -> Sem r ()
deleteWhen p as@StoreList{..} = do
    len <- length as
    delete' 0 (len - 1)
  where
    delete' n end  =
      if n > end
        then pure ()
        else do
          mRes <- as !! n
          case mRes of
            Nothing -> delete' (n + 1) end
            Just res ->
              if p res
                then let k = RawStoreKey
                           { rsKey = elementKey n
                           , rsStoreKey = storeListKey
                           }
                     in storeDelete k >> delete' (n + 1) end
                else delete' (n + 1) end

-- | Get the first index where an element appears in the list.
elemIndex
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => Eq a
  => a
  -> StoreList a
  -> Sem r (Maybe Idx)
elemIndex a as = do
    len <- length as
    elemIndex' 0 len
  where
    elemIndex' n len
      | n == len = pure Nothing
      | otherwise = do
          mRes <- as !! n
          let keepLooking = elemIndex' (n + 1) len
          case mRes of
            Nothing -> keepLooking
            Just a' -> if a == a' then pure $ Just (Idx n) else keepLooking

foldl
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => (b -> a -> b)
  -> b
  -> StoreList a
  -> Sem r b
foldl f b as = do
  len <- length as
  foldl' 0 len b
  where
    foldl' currentIndex end accum
      | currentIndex == end = pure accum
      | currentIndex < end = do
          ma <- as !! currentIndex
          case ma of
            Nothing -> foldl' (currentIndex + 1) end accum
            Just a  -> foldl' (currentIndex + 1) end (f accum a)
      | otherwise = error "Impossible case in StoreList foldl!"

-- | View the 'StoreList' as a 'List'.
toList
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => StoreList a
  -> Sem r [a]
toList = foldl (flip (:)) []

--------------------------------------------------------------------------------
-- Internal functions
--------------------------------------------------------------------------------

-- NOTE: Many things in this module are completely ad hoc, but tries to follow
-- the patterns set in https://github.com/cosmos/cosmos-sdk/blob/master/store/list/list.go
-- for future compatability, and because this ad-hoc-ness doesn't leak out.

lengthKey :: BS.ByteString
lengthKey = Hex.toBytes "0x00"

elementKey
  :: Idx
  -> BS.ByteString
elementKey (Idx k) =
    let padToNChars n a =
          let nZeros = n - P.length a
          in replicate nZeros '0' <> a
    in Hex.toBytes "0x01" <> cs (padToNChars 20 $ show k)

unElementKey
  :: BS.ByteString
  -> Idx
unElementKey bs = 
    let str = cs $ fromMaybe (error "Idx missing 0x1 prefix") $ 
                BS.stripPrefix (Hex.toBytes "0x01") bs
    in Idx . read . dropWhile (== '0') $ str
        
length
  :: Members [Error AppError, ReadStore] r
  => StoreList a
  -> Sem r Word64
length StoreList{..} =
  let k = RawStoreKey
        { rsStoreKey = storeListKey
        , rsKey = lengthKey
        }
      unsafeDecode v = case decode v of
        Left e -> throwSDKError (ParseError $ "Impossible codec error: "  <> cs e)
        Right a -> pure a
  in storeGet k >>= maybe (pure 0) unsafeDecode

writeAt
  :: Members [Error AppError, ReadStore, WriteStore] r
  => Idx
  -> BS.ByteString
  -> StoreList a
  -> Sem r ()
writeAt idx@(Idx i) a as@StoreList{..} = do
  len <- length as
  writeAt' len
  where
    writeAt' len
      | i == len = do
        let k = RawStoreKey
              { rsStoreKey = storeListKey
              , rsKey = elementKey i
              }
            lk = RawStoreKey
              { rsStoreKey = storeListKey
              , rsKey = lengthKey
              }
        storePut k a
        storePut lk (encode i)
      | i < len = do
        let k = RawStoreKey
              { rsStoreKey = storeListKey
              , rsKey = elementKey i
              }
        storePut k a
      | otherwise = throwSDKError $ InternalError "Cannot write past list length index."
