module Tendermint.SDK.StoreExampleSpec where

import           Control.Lens ((^.), to)
import           Data.ByteArray.HexString             (toBytes, fromBytes)
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Test.Hspec
import           Data.Proxy
import Servant.API
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Store
import           Tendermint.SDK.StoreExample
import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Types
import           Tendermint.SDK.Router
import           Tendermint.SDK.StoreQueries

spec :: Spec
spec =
  describe "UserStore" $ do
    it "should serve basic routes" $ do
      rawStore <- mkAuthTreeStore
      let userStore :: UserStore
          userStore = Store { storeRawStore = rawStore }

          userServer :: RouteT (QueryApi UserStoreContents) IO 
          userServer = storeQueryHandlers (Proxy :: Proxy UserStoreContents) userStore

          userApi :: Proxy ("user" :> QueryApi UserStoreContents)
          userApi = Proxy

          serveRoutes :: Application IO
          serveRoutes = serve userApi (Proxy :: Proxy IO) userServer

      let 
          expectedMan  = User "1" "man"
          manKey = UserKey "abcd"
          expectedDog  = Dog "2" "fido"
          dogKey = DogKey "1234"

      putUser manKey expectedMan userStore
      putDog dogKey expectedDog userStore

      let
          manKeyHex = manKey ^. rawKey . to fromBytes
          dogKeyHex = dogKey ^. rawKey . to fromBytes
          manQuery = Request.Query manKeyHex "user/user" 0 False
          dogQuery = Request.Query dogKeyHex "user/dog" 0 False

      qManRes  <- serveRoutes manQuery
      qDogRes  <- serveRoutes dogQuery

      let man = decode . toBytes . Response.queryValue $ qManRes
          dog = decode . toBytes . Response.queryValue $ qDogRes
      man `shouldBe` Right expectedMan
      dog `shouldBe` Right expectedDog


