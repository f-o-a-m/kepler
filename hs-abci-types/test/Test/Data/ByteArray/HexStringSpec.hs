module Test.Data.ByteArray.HexStringSpec where

import           Data.ByteArray.HexString (HexString)
import           Data.String              (IsString (..))
import           Test.Hspec
spec :: Spec
spec = do
  describe "HexString" $ do
    it "Works with any mixed casing of characters" $
      let
        hs :: String -> HexString
        hs = fromString
      in allEqual
        [ hs "0xAA"
        , hs "0xAa"
        , hs "0xaA"
        , hs "0xaa"
        ]


allEqual :: Eq a => [a] -> Bool
allEqual [] = False
allEqual (x:xs) = go xs
  where
    go []     = True
    go (y:ys) = if x == y then go ys else False
