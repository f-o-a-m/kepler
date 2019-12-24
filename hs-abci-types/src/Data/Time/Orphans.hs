-- vendored from: http://hackage.haskell.org/package/time-1.9.3/docs/src/Data.Time.Format.Format.Class.html
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Time.Orphans () where

import           Data.Fixed                 (Pico)
import           Data.Time.Calendar.Private (PadOption (..), ShowPadded (..),
                                             quotBy, remBy)
import           Data.Time.Clock            (DiffTime)
import           Data.Time.Format
import           Data.Traversable           (for)
import           Text.Read                  (readMaybe)

type FormatNumericPadding = Maybe Char

getPadOption
  :: Bool
  -> Bool
  -> Int
  -> Char
  -> Maybe FormatNumericPadding
  -> Maybe Int
  -> PadOption
getPadOption trunc fdef idef cdef mnpad mi = let
    c = case mnpad of
        Just (Just c') -> c'
        Just Nothing   -> ' '
        _              -> cdef
    i = case mi of
        Just i' -> case mnpad of
            Just Nothing -> i'
            _            -> if trunc then i' else max i' idef
        Nothing -> idef
    f = case mi of
        Just _ -> True
        Nothing -> case mnpad of
            Nothing       -> fdef
            Just Nothing  -> False
            Just (Just _) -> True
    in if f then Pad i c else NoPad

formatGeneral
  :: Bool
  -> Bool
  -> Int
  -> Char
  -> (TimeLocale -> PadOption -> t -> String)
  -> (TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String)
formatGeneral trunc fdef idef cdef ff tl np w = ff tl $
  getPadOption trunc fdef idef cdef np w

formatNumber
  :: (ShowPadded i)
  => Bool
  -> Int
  -> Char
  -> (t -> i)
  -> (TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String)
formatNumber fdef idef cdef ff = formatGeneral False fdef idef cdef $ \_ pado -> showPaddedNum pado . ff

formatNumberStd
  :: Int
  -> (t -> Integer)
  -> (TimeLocale -> Maybe NumericPadOption -> Maybe Int -> t -> String)
formatNumberStd n = formatNumber False n '0'

instance FormatTime DiffTime where
    formatCharacter 'w' = Just $ formatNumberStd 1 $ quotBy $ 7 * 86400
    formatCharacter 'd' = Just $ formatNumberStd 1 $ quotBy 86400
    formatCharacter 'D' = Just $ formatNumberStd 1 $ remBy 7 . quotBy 86400
    formatCharacter 'h' = Just $ formatNumberStd 1 $ quotBy 3600
    formatCharacter 'H' = Just $ formatNumberStd 2 $ remBy 24 . quotBy 3600
    formatCharacter 'm' = Just $ formatNumberStd 1 $ quotBy 60
    formatCharacter 'M' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 60
    -- these are weird
    formatCharacter 's' = Just $ formatNumberStd 1 $ quotBy 1
    formatCharacter 'S' = Just $ formatNumberStd 2 $ remBy 60 . quotBy 1
    formatCharacter _   = Nothing

buildTimeDays :: [(Char,String)] -> Maybe Integer
buildTimeDays xs = do
    tt <- for xs $ \(c,s) -> case c of
        'w' -> fmap ((*) 7) $ readMaybe s
        'd' -> readMaybe s
        'D' -> readMaybe s
        _   -> return 0
    return $ sum tt

buildTimeSeconds :: [(Char,String)] -> Maybe Pico
buildTimeSeconds xs = do
    tt <- for xs $ \(c,s) -> let
        readInt :: Integer -> Maybe Pico
        readInt t = do
            i <- readMaybe s
            return $ fromInteger $ i * t
        in case c of
            'h' -> readInt 3600
            'H' -> readInt 3600
            'm' -> readInt 60
            'M' -> readInt 60
            's' -> readMaybe s
            'S' -> readMaybe s
            _   -> return 0
    return $ sum tt

instance ParseTime DiffTime where
    buildTime _ xs = do
        dd <- buildTimeDays xs
        tt <- buildTimeSeconds xs
        return $ (fromInteger dd * 86400) + realToFrac tt
