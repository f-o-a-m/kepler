module SimpleStorage.Test.HandlersSpec where

import           Control.Lens                         (to, (&), (.~), (^.))
import           Control.Lens.Wrapped                 (_Unwrapped', _Wrapped')
import           Data.Binary                          (decode, encode)
import           Data.ByteArray.Base64String          (Base64String)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Int                             (Int32)
import           Data.ProtoLens                       (defMessage)
import           Data.ProtoLens.Encoding              (encodeMessage)
import           Data.Text                            (pack)
import           Network.ABCI.Server.App              (Request (..),
                                                       Response (..))
import qualified Network.ABCI.Types.Messages.Request  as Req
import qualified Network.ABCI.Types.Messages.Response as Resp
import           SimpleStorage.Application            (AppConfig, Handler,
                                                       makeAppConfig,
                                                       runHandler,
                                                       transformHandler)
import           SimpleStorage.Handlers               (deliverTxH, queryH)
import           SimpleStorage.Logging
import qualified SimpleStorage.Modules.SimpleStorage  as SS
import           SimpleStorage.Types                  (UpdateCountTx (..))
import           Tendermint.SDK.Module
import           Tendermint.SDK.Store
import           Test.Hspec
import           Test.QuickCheck


spec :: Spec
spec = beforeAll beforeAction $ do
  describe "SimpleStorage E2E - via handlers" $ do
    it "Can query the initial count and make sure it's 0" $ \(cfg, io) -> do
      let handle = transformHandler cfg . queryH io
      (ResponseQuery queryResp) <- handle
        ( RequestQuery $ defMessage ^. _Unwrapped'
           & Req._queryPath .~ "count/count"
           & Req._queryData  .~ SS.CountKey ^. rawKey . to Base64.fromBytes
        )
      let foundCount = queryResp ^. Resp._queryValue . to decodeCount
      foundCount `shouldBe` 0
    it "Can update count and make sure it's increments" $ \(cfg, io) -> do
      genUsername <- pack . getPrintableString <$> generate arbitrary
      genCount    <- abs <$> generate arbitrary
      let
        handleDeliver = transformHandler cfg . deliverTxH io
        handleQuery = transformHandler cfg . queryH io
        updateTx = (defMessage ^. _Unwrapped') { updateCountTxUsername = genUsername
                                               , updateCountTxCount = genCount
                                               }
        encodedUpdateTx = Base64.fromBytes $ encodeMessage (updateTx ^. _Wrapped')
      (ResponseDeliverTx deliverResp) <- handleDeliver
        (  RequestDeliverTx
        $  defMessage
        ^. _Unwrapped'
        &  Req._deliverTxTx
        .~ encodedUpdateTx
        )
      (deliverResp ^. Resp._deliverTxCode) `shouldBe` 0
      (ResponseQuery queryResp) <- handleQuery
        ( RequestQuery $ defMessage ^. _Unwrapped'
            & Req._queryPath .~ "count/count"
            & Req._queryData  .~ SS.CountKey ^. rawKey . to Base64.fromBytes
        )
      let foundCount = queryResp ^. Resp._queryValue . to decodeCount
      foundCount `shouldBe` genCount

beforeAction :: IO (AppConfig, TendermintIO SS.Query SS.Message SS.Api Handler)
beforeAction = do
  cfg <- mkLogConfig "handler-spec" >>= makeAppConfig
  io <- runHandler cfg $ runApp SS.simpleStorageComponent ()
  pure (cfg, io)


encodeCount :: Int32 -> Base64String
encodeCount = Base64.fromBytes . LBS.toStrict . encode

decodeCount :: Base64String -> Int32
decodeCount = decode . LBS.fromStrict . Base64.toBytes
