{-# LANGUAGE NoImplicitPrelude #-}

module Tendermint.SDK.BaseApp.Store.Array
  ( Array
  , makeArray
  , append
  , modifyAtIndex
  , deleteWhen
  , (!!)
  , elemIndex
  , toList
  ) where

import           Control.Lens                  (iso, (^.))
import qualified Data.ByteArray.HexString      as Hex
import qualified Data.ByteString               as BS
import           Data.Maybe                    (fromMaybe)
import           Data.String.Conversions       (cs)
import           Data.Word                     (Word64)
import           Polysemy
import           Polysemy.Error                (Error)
import           Prelude                       hiding (foldl, length, (!!))
import qualified Prelude                       as P (length)
import           Tendermint.SDK.BaseApp.Errors (AppError,
                                                SDKError (InternalError),
                                                throwSDKError)
import           Tendermint.SDK.BaseApp.Store  (IsKey (..), KeyRoot (..),
                                                RawKey (..), ReadStore, Store,
                                                WriteStore, delete, get,
                                                makeStore, nestStore, put,
                                                rawKey)
import           Tendermint.SDK.Codec          (HasCodec (..))



-- | A 'Array a' is an appendable list whose elements can be accessed
-- | by their index. You can also delete from the list, in which case accessing
-- | that index will result in a `Nothing`.
data Array (a :: *) = Array
  { listStore :: Store (Array a) }

-- | Represents an index into a list
newtype Idx = Idx Word64 deriving (Eq, Show, Ord, Num)

instance RawKey Idx where
  rawKey = iso elementKey unElementKey

instance IsKey Idx (Array a) where
  type Value Idx (Array a) = a

-- Internal, used for accessing list length.
data LengthKey = LengthKey

instance RawKey LengthKey where
  rawKey = iso (const lengthKey) unLengthKey

instance IsKey LengthKey (Array a) where
  type Value LengthKey (Array a) = Word64

-- | Smart constuctor to make sure we're making a 'Array' from
-- | the appropriate key type.
makeArray
  :: IsKey k ns
  => Value k ns ~ Array a
  => k
  -> Store ns
  -> Value k ns
makeArray k store =
  let skr :: KeyRoot (Array a)
      skr = KeyRoot $ k ^. rawKey
  in Array $ nestStore store (makeStore skr)

-- | Add an item to the end of the list.
append
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => a
  -> Array a
  -> Sem r ()
append a as@Array{..} = do
  n <- length as
  writeAt (Idx n) a as
  put listStore LengthKey (n + 1)

-- | Access an item directly by its index.
(!!)
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => Array a
  -> Idx
  -> Sem r (Maybe a)
as@Array{..} !! n = do
  len <- length as
  if Idx (len - 1) < n
    then pure Nothing
    else get listStore n

infixl 9  !!

-- | Modify a list at a particular index, return the
-- | updated value if the element was found.
modifyAtIndex
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => Idx
  -> (a -> a)
  -> Array a
  -> Sem r (Maybe a)
modifyAtIndex i f as = do
  mRes <- as !! i
  case mRes of
    Nothing -> pure Nothing
    Just res -> do
      let a' = f res
      writeAt i a' as
      pure (Just a')

-- | Delete when the predicate evaluates to true.
deleteWhen
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => (a -> Bool)
  -> Array a
  -> Sem r ()
deleteWhen p as@Array{..} = do
    len <- length as
    delete' 0 (Idx (len - 1))
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
                then do
                  delete listStore n
                  delete' (n + 1) end
                else delete' (n + 1) end

-- | Get the first index where an element appears in the list.
elemIndex
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => Eq a
  => a
  -> Array a
  -> Sem r (Maybe Idx)
elemIndex a as = do
    len <- length as
    elemIndex' 0 (Idx len)
  where
    elemIndex' n len
      | n == len = pure Nothing
      | otherwise = do
          mRes <- as !! n
          let keepLooking = elemIndex' (n + 1) len
          case mRes of
            Nothing -> keepLooking
            Just a' -> if a == a' then pure $ Just n else keepLooking

foldl
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => (b -> a -> b)
  -> b
  -> Array a
  -> Sem r b
foldl f b as = do
  len <- length as
  foldl' 0 (Idx len) b
  where
    foldl' currentIndex end accum
      | currentIndex == end = pure accum
      | currentIndex < end = do
          ma <- as !! currentIndex
          case ma of
            Nothing -> foldl' (currentIndex + 1) end accum
            Just a  -> foldl' (currentIndex + 1) end $! f accum a
      | otherwise = error "Impossible case in Array foldl!"

-- | View the 'Array' as a 'Array'.
toList
  :: Members [Error AppError, ReadStore] r
  => HasCodec a
  => Array a
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

unLengthKey :: BS.ByteString -> LengthKey
unLengthKey bs
  | bs == Hex.toBytes "0x00" = LengthKey
  | otherwise = error $ "Couldn't parse LengthKey bytes: " <> cs bs

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
    let str = cs $ fromMaybe (error "Idx missing 0x01 prefix") $
                BS.stripPrefix (Hex.toBytes "0x01") bs
    in Idx . read . dropWhile (== '0') $ str

length
  :: Members [Error AppError, ReadStore] r
  => Array a
  -> Sem r Word64
length Array{..} = do
  mLen <- get listStore LengthKey
  pure $ fromMaybe 0 mLen

writeAt
  :: Members [Error AppError, ReadStore, WriteStore] r
  => HasCodec a
  => Idx
  -> a
  -> Array a
  -> Sem r ()
writeAt idx@(Idx i) a as@Array{..} = do
  len <- length as
  writeAt' len
  where
    writeAt' len
      | i == len = do
        put listStore idx a
        put listStore LengthKey i
      | i < len =
        put listStore idx a
      | otherwise = throwSDKError $ InternalError "Cannot write past list length index."
