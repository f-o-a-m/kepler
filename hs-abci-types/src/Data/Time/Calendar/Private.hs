-- venored from: http://hackage.haskell.org/package/time-1.9.3/docs/src/Data.Time.Calendar.Private.html

module Data.Time.Calendar.Private
  ( PadOption (..)
  , ShowPadded (..)
  , quotBy
  , remBy
  ) where

data PadOption = Pad Int Char | NoPad

showPadded :: PadOption -> String -> String
showPadded NoPad s     = s
showPadded (Pad i c) s = replicate (i - length s) c ++ s

class (Num t,Ord t,Show t) => ShowPadded t where
    showPaddedNum :: PadOption -> t -> String

instance ShowPadded Integer where
    showPaddedNum NoPad i = show i
    showPaddedNum pad i   | i < 0 = '-':(showPaddedNum pad (negate i))
    showPaddedNum pad i   = showPadded pad $ show i

instance ShowPadded Int where
    showPaddedNum NoPad i = show i
    showPaddedNum _pad i  | i == minBound = show i
    showPaddedNum pad i   | i < 0 = '-':(showPaddedNum pad (negate i))
    showPaddedNum pad i   = showPadded pad $ show i

quotBy :: (Real a,Integral b) => a -> a -> b
quotBy d n = truncate ((toRational n) / (toRational d))

remBy :: Real a => a -> a -> a
remBy d n = n - (fromInteger f) * d where
    f = quotBy d n
