module SimpleStorage.Test.E2ESpec where

import           Control.Lens                         (to, (^.))
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.Binary                          (decode, encode)
import qualified Data.ByteArray.HexString             as Hex
import qualified Data.ByteString.Lazy                 as LBS
import           Data.Default.Class                   (def)
import           Data.Int                             (Int32)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Response as Resp
import qualified Network.Tendermint.Client            as RPC
import           Test.Hspec


spec :: Spec
spec = do
  describe "SimpleStorage E2E - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    it "Can query the initial count and make sure it's 0" $ do
      pendingWith "Pending hs-tendermint-client resolution."
      let queryReq =
            def { RPC.requestABCIQueryPath = Just "count"
                }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      let foundCount = queryResp ^. Resp._queryValue . to decodeCount
      foundCount `shouldBe` 0

encodeCount :: Int32 -> Hex.HexString
encodeCount = Hex.fromBytes . LBS.toStrict . encode

decodeCount :: Hex.HexString -> Int32
decodeCount =  decode . LBS.fromStrict . Hex.toBytes

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")
