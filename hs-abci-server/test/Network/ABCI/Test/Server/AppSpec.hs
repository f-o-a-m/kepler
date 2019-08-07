module Network.ABCI.Test.Server.AppSpec where

import           Data.Bifunctor                      (first)
import qualified Data.Binary.Put                     as Put
import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import           Data.Either                         (isLeft)
import           Network.ABCI.Server.App
import qualified Network.ABCI.Server.App.DecodeError as DecodeError
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "LPByteStrings" $ do
    let decode = first DecodeError.print . decodeLengthPrefix
    it "decoding and encoded bytestrings yields same bytestrings" $
      property $ \listOfListOfbyte ->
        let byteStrings = map BS.pack listOfListOfbyte
        in Right byteStrings == decode (encodeLengthPrefix byteStrings)
    it "decode fails gracefully when given a string larger than maxMessageLen" $
      let ginormousSizeVarLen = LPByteStrings $ 8 `BS.cons` runPut (Put.putWord64be maxBound)
      in decode ginormousSizeVarLen `shouldSatisfy` isLeft


runPut :: Put.Put -> BS.ByteString
runPut = LBS.toStrict . Put.runPut
