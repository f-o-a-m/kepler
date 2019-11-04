module KVStore.Test.KVSpec where

import Control.Monad.Catch (try)
import Data.Either (isRight)
import           Control.Lens                         (to,(^.))
import           Data.Aeson                           (ToJSON)
import           Data.Aeson.Encode.Pretty             (encodePretty)
import           Data.ByteArray.Base64String          (Base64String)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteArray.HexString             as Hex
import           Data.ByteString             (ByteString)
import           Data.Default.Class                   (def)
import           Data.String.Conversions              (cs)
import qualified Network.ABCI.Types.Messages.Response as Response
import qualified Network.Tendermint.Client            as RPC
import           Test.Hspec


spec :: Spec
spec = do
  describe "Tendermint KV Store - via hs-tendermint-client" $ do

    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    it "Can query /abci_info and parse the result" $ do
      result :: Either IOError RPC.ResultABCIInfo <- try $ runRPC RPC.abciInfo
      result `shouldSatisfy` isRight

    it "Can query /block and parse the result" $ do
      -- @NOTE: this defaults to latest block
      result :: Either IOError RPC.ResultBlock <- try $ runRPC (RPC.block def)
      result `shouldSatisfy` isRight

    it "Can submit a tx and make sure the response code is 0 (success)" $ do
      -- set name key
      let txReq = RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeName "name=satoshi" }
      deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $
        RPC.broadcastTxCommit txReq
      let deliverRespCode = deliverResp ^. Response._deliverTxCode
      deliverRespCode `shouldBe` 0
      -- get its value
      let dName = Hex.fromBytes $ cs @String @ByteString "name"
          queryReq = def { RPC.requestABCIQueryData = dName }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      let foundName = queryResp ^. Response._queryValue . to decodeName
      foundName `shouldBe` "satoshi"

    -- it "Can query /tx and parse the result" $ do
    --   pure ()

    -- it "Can query /tx and parse the result with a proof" $ do
    --   pure ()

encodeName :: String -> Base64String
encodeName = Base64.fromBytes . cs @String @ByteString

decodeName :: Base64String -> String
decodeName = cs @ByteString @String . Base64.toBytes

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")
