module Nameservice.Test.E2ESpec where

import           Control.Lens                          (to, (^.))
import           Data.Aeson                            (ToJSON)
import           Data.Aeson.Encode.Pretty              (encodePretty)
import qualified Data.ByteArray.HexString              as Hex
import           Data.Default.Class                    (def)
import           Data.String.Conversions               (cs)
import           Nameservice.Modules.Nameservice.Types (Name (..))
import qualified Network.Tendermint.Client             as RPC
import           Tendermint.SDK.Store                  (rawKey)
import           Tendermint.SDK.Types.Address          (Address (..),
                                                        addressToBytes)
import qualified Data.ByteArray.Base64String          as Base64
import qualified Data.ByteString.Lazy as BL
import Proto3.Suite (Message, toLazyByteString)
import Nameservice.Modules.Nameservice.Messages (NameserviceMessage(..),
                                                 SetName(..),
                                                 BuyName(..),
                                                 DeleteName(..))
import           Test.Hspec

spec :: Spec
spec = do
  let satoshi = Name "satoshi"
      addr1 = Address "0x01"
      addr2 = Address "0x02"
  describe "Nameservice Spec" $ do
    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

    it "Can create a name" $ do
      let msg = BuyName satoshi "hello world" addr1 1
          txReq =
            RPC.RequestBroadcastTxCommit { RPC.requestBroadcastTxCommitTx = encodeTxMsg msg }
      deliverResp <- fmap RPC.resultBroadcastTxCommitDeliverTx . runRPC $ RPC.broadcastTxCommit txReq
      pending

    it "Can query for a name" $ do
      let queryReq = def { RPC.requestABCIQueryPath = Just "nameservice/whois"
                         , RPC.requestABCIQueryData = satoshi ^. rawKey . to Hex.fromBytes
                         }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      pending

    it "Can query for a name that doesn't exist" $ do
      pending

    it "Can query balances" $ do
      let aAddress = Address undefined
          queryReq = def { RPC.requestABCIQueryPath = Just "token/balance"
                         , RPC.requestABCIQueryData = undefined
                         }
      queryResp <- fmap RPC.resultABCIQueryResponse . runRPC $
        RPC.abciQuery queryReq
      pending

    it "Can set a name value" $ do
      
      pending

    it "Can buy a name" $ do
      pending

    it "Can fail to buy a name" $ do
      pending

    it "Can fail a transfer" $ do
      pending

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig =
      let RPC.Config baseReq _ _ = RPC.defaultConfig "localhost" 26657
          prettyPrint :: forall b. ToJSON b => String -> b -> IO ()
          prettyPrint prefix a = putStrLn $ prefix <> "\n" <> (cs . encodePretty $ a)
      in RPC.Config baseReq (prettyPrint "RPC Request") (prettyPrint "RPC Response")

encodeTxMsg :: Message a => a -> Base64.Base64String
encodeTxMsg = Base64.fromBytes . BL.toStrict . toLazyByteString
