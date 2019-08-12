module SimpleStorage.Test.HandlersSpec where

import           Control.Lens                         (to, (^.))
import           Control.Lens.Wrapped                 (_Unwrapped')
import           Data.Binary                          (decode, encode)
import qualified Data.ByteArray.HexString             as Hex
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Int                             (Int32)
import           Data.ProtoLens                       (defMessage)
import           Network.ABCI.Server.App              (Request (..),
                                                       Response (..))
import qualified Network.ABCI.Types.Messages.Response as Resp
import           SimpleStorage.Application            (makeAppConfig,
                                                       transformHandler)
import           SimpleStorage.Handlers               (queryH)
import           Test.Hspec


spec :: Spec
spec = beforeAll makeAppConfig $ do
  describe "SimpleStorage E2E - via handlers" $ do
    it "Can query the initial count and make sure it's 0" $ \cfg -> do
      let handle = transformHandler cfg . queryH
      (ResponseQuery queryResp) <- handle (RequestQuery $ defMessage ^. _Unwrapped')
      print queryResp
      let foundCount = queryResp ^. Resp._queryValue . to decodeCount
      foundCount `shouldBe` 0

encodeCount :: Int32 -> Hex.HexString
encodeCount 0 = "0x"
encodeCount c = Hex.fromBytes . LBS.toStrict . encode $ c

decodeCount :: Hex.HexString -> Int32
decodeCount "0x" = 0
decodeCount hs   =  decode . LBS.fromStrict . Hex.toBytes $ hs
