module Tendermint.SDK.StoreExampleSpec where

import           Data.ByteArray.HexString             (toBytes)
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Test.Hspec

import           Tendermint.SDK.Codec
import           Tendermint.SDK.StoreExample

spec :: Spec
spec =
  describe "UserStore" $ do
    it "should serve basic routes" $ do
      let manQuery = Request.Query "0xabcd" "user/user" 0 False
          dogQuery = Request.Query "0xabcd" "user/dog" 0 False
          expectedMan  = User "1" "man"
          expectedDog  = User "2" "dog"
          decode = codecDecode userCodec
      qManRes  <- serveRoutes manQuery
      qDogRes  <- serveRoutes dogQuery
      let (Right man) = decode . toBytes . Response.queryValue $ qManRes
          (Right dog) = decode . toBytes . Response.queryValue $ qDogRes
      man `shouldBe` expectedMan
      dog `shouldBe` expectedDog


