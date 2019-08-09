module SimpleStorage.Test.E2ESpec where

import qualified Network.Tendermint.Client as RPC
import           Test.Hspec

spec :: Spec
spec = do
  describe "SimpleStorage E2E" $ do
    it "Can query /health to make sure the node is alive" $ do
      resp <- runRPC RPC.health
      resp `shouldBe` RPC.ResultHealth

runRPC :: forall a. RPC.TendermintM a -> IO a
runRPC = RPC.runTendermintM rpcConfig
  where
    rpcConfig :: RPC.Config
    rpcConfig = RPC.defaultConfig "localhost" 26657
