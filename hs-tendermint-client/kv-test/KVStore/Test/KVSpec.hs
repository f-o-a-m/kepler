module KVStore.Test.KVSpec where

-- import           Control.Lens                         (to, (^.))
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
-- import           Data.Binary                          (decode, encode)
-- import           Data.ByteArray.Base64String          (Base64String)
-- import qualified Data.ByteArray.Base64String          as Base64
-- import qualified Data.ByteArray.HexString             as Hex
-- import qualified Data.ByteString.Lazy                 as LBS
import           Data.Default.Class                   (def)
-- import           Data.Int                             (Int32)
import           Data.String.Conversions              (cs)
-- import qualified Network.ABCI.Types.Messages.Response as Resp
-- import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Test.Hspec


spec :: Spec
spec = do
  describe "Tendermint KV Store - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    -- the following are just testing for parse errors
    it "Can query /abci_info and parse the result" $ do
      _ <- runRPC RPC.abciInfo
      pure ()

    it "Can query /block and parse the result" $ do
      -- @NOTE: defaults to latest block
      _ <- runRPC $ RPC.block def
      pure ()

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")
