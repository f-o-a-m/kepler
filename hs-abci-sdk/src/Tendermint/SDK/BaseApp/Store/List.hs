{-# LANGUAGE NoImplicitPrelude #-}

module Tendermint.SDK.BaseApp.Store.List
  ( StoreList
  , append
  , (!!)
  , elemIndex
  , delete
  , toList
  ) where

import           Control.Monad                 (forM)
import qualified Data.ByteArray.HexString      as Hex
import qualified Data.ByteString               as BS
import           Data.Maybe                    (catMaybes)
import           Data.String.Conversions       (cs)
import           Data.Word                     (Word64)
import           Polysemy
import           Polysemy.Error                (Error)
import           Prelude                       hiding (length, (!!))
import qualified Prelude                       as P (length)
import           Tendermint.SDK.BaseApp.Errors (AppError, SDKError (InternalError, ParseError),
                                                throwSDKError)
import           Tendermint.SDK.BaseApp.Store  (RawStoreKey (..), ReadStore,
                                                WriteStore, storeGet, storePut)
import           Tendermint.SDK.Codec          (HasCodec (..))


-- NOTE: Many things in this module are completely ad hoc, but tries to follow
-- the patterns set in https://github.com/cosmos/cosmos-sdk/blob/master/store/list/list.go
-- for future compatability, and because this ad-hoc-ness doesn't leak out.

lengthKey :: BS.ByteString
lengthKey = Hex.toBytes "0x00"

elementKey
  :: Word64
  -> BS.ByteString
elementKey k =
    let padToNChars n a =
          let nZeros = n - P.length a
          in replicate nZeros '0' <> a
    in Hex.toBytes "0x01" <> cs (padToNChars 20 $ show k)

data StoreList a = StoreList
  { storeListKey :: BS.ByteString
  }

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
  => Word64
  -> BS.ByteString
  -> StoreList a
  -> Sem r ()
writeAt i a as@StoreList{..} = do
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

-- | Add an item to the end of the list.
append
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => a
  -> StoreList a
  -> Sem r ()
append a as = do
  n <- length as
  writeAt n (encode a) as

-- | Access an item directly by it's index.
(!!)
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => StoreList a
  -> Word64
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
          Left e ->
             if BS.null res
               then pure Nothing
               else throwSDKError (ParseError $ "Impossible codec error: "  <> cs e)

infixl 9  !!

-- | Delete the first occurance of an item.
delete
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => Eq a
  => a
  -> StoreList a
  -> Sem r ()
delete a as = do
    len <- length as
    delete' 0 (len - 1)
  where
    delete' n maxN  =
      if n > maxN
        then pure ()
        else do
          mRes <- as !! n
          case mRes of
            Nothing -> delete' (n + 1) maxN
            Just res ->
              if a == res
                then writeAt n mempty as
                else delete' (n + 1) maxN

-- | Get the first index where an element appears in the list.
elemIndex
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => Eq a
  => a
  -> StoreList a
  -> Sem r (Maybe Word64)
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
            Just a' -> if a == a' then pure $ Just n else keepLooking

-- | View the 'StoreList' as a 'List'.
toList
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => StoreList a
  -> Sem r [a]
toList as = do
  n <- length as
  catMaybes <$> forM [0..n] ((!!) as)
