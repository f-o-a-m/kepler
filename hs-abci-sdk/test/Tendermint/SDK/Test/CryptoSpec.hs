module Tendermint.SDK.Test.CryptoSpec (spec) where

import           Crypto.Secp256k1                 (CompactRecSig (..), SecKey,
                                                   derivePubKey,
                                                   exportCompactRecSig, secKey)
import qualified Data.ByteArray.HexString         as Hex
import           Data.ByteString                  (ByteString, snoc)
import           Data.ByteString.Short            (fromShort)
import           Data.Maybe                       (fromJust)
import           Data.Proxy
import           Data.String                      (fromString)
import           Tendermint.SDK.Crypto            (Secp256k1)
import           Tendermint.SDK.Types.Transaction
import           Test.Hspec

spec :: Spec
spec = describe "Crypto Tests" $ do
    it "Can sign a transaction and recover the signature" $ do
      let rawTxWithoutSig = RawTransaction
            { rawTransactionData = "abcd"
            , rawTransactionSignature = ""
            , rawTransactionRoute= "dog"
            , rawTransactionGas = 10
            , rawTransactionNonce = 0
            }
          signature = signRawTransaction algProxy privateKey rawTxWithoutSig
          rawTxWithSig = rawTxWithoutSig {rawTransactionSignature =
                           encodeCompactSig $ exportCompactRecSig signature}
          eTx = parseTx algProxy rawTxWithSig
          Tx{..} = case eTx of
            Left errMsg -> error $ show errMsg
            Right a     -> a
      txSigner `shouldBe` derivePubKey privateKey

privateKey :: SecKey
privateKey = fromJust . secKey . Hex.toBytes . fromString $
  "f65255094d7773ed8dd417badc9fc045c1f80fdc5b2d25172b031ce6933e039a"

algProxy :: Proxy Secp256k1
algProxy = Proxy

encodeCompactSig :: CompactRecSig -> ByteString
encodeCompactSig (CompactRecSig r s v) = snoc (fromShort r <> fromShort s) v
