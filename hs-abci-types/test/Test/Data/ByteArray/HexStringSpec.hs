{-# LANGUAGE TypeApplications #-}

module Test.Data.ByteArray.HexStringSpec where

import           Data.ByteArray.HexString (HexString, fromBytes, toBytes)
import           Data.ByteString
import qualified Data.List                as L
import           Test.Hspec
spec :: Spec
spec = do
  describe "HexString" $ do
    it "Works with any mixed casing of characters" $ do
      let vals = [ "0xAA", "0xAa", "0xaA", "0xaa"] :: [HexString]
      L.length (L.nub vals) `shouldBe` 1

    it "has inverese fromBytes and toBytes" $ do
      let testHx = "0x1234567890" :: HexString
          testBS = "abcdefghijkl" :: ByteString
      (toBytes . fromBytes $ testBS) `shouldBe` testBS
      (fromBytes @ByteString . toBytes $ testHx) `shouldBe` testHx
