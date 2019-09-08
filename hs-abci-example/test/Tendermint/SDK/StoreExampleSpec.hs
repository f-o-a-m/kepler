module Tendermint.SDK.StoreExampleSpec where

import           Control.Lens ((^.), to)
import           Data.ByteArray.HexString             (toBytes, fromBytes)
import           Test.Hspec
import           Data.Proxy
import           Test.QuickCheck.Gen (generate)
import           Test.QuickCheck.Arbitrary (arbitrary)
import Servant.API
import qualified Network.ABCI.Types.Messages.Response            as Response
import qualified Network.ABCI.Types.Messages.Request             as Request
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Store
import           Tendermint.SDK.StoreExample
import           Tendermint.SDK.Router.Class
import           Tendermint.SDK.Router.Types
import           Tendermint.SDK.Router
import           Tendermint.SDK.StoreQueries
import           Tendermint.SDK.StoreExample.Instances ()

type API = "dog"  :> QueryApi DogStoreContents
      :<|> "user" :> QueryApi UserStoreContents


spec :: Spec
spec =
  describe "UserStore" $ do
    it "should serve basic routes" $ do
      rawStore <- mkAuthTreeStore

      -- | Define the stores
      let userStore :: UserStore
          userStore = Store { storeRawStore = rawStore }

          userServer :: RouteT ("user" :> QueryApi UserStoreContents) IO
          userServer = allStoreHandlers userStore

          dogStore :: DogStore
          dogStore = Store { storeRawStore = rawStore }

          dogServer :: RouteT ("dog" :> QueryApi DogStoreContents) IO
          dogServer = allStoreHandlers dogStore

          api :: Proxy API
          api = Proxy

          serveRoutes :: Application IO
          serveRoutes = serve api (Proxy :: Proxy IO) (dogServer :<|> userServer)

      -- | Fill the stores
      ownerKey <- generate arbitrary
      expectedOwner <- generate arbitrary
      -- let
      --     expectedMan  = User "1" "man"
      --     manKey = UserKey "abcd"
      --     expectedDog  = Dog "2" "fido"
      --     dogKey = DogKey "1234"

      putOwner ownerKey expectedOwner userStore
      -- putDog dogKey expectedDog userStore

      let
          ownerKeyHex = ownerKey ^. rawKey . to fromBytes
          ownerQuery = Request.Query ownerKeyHex "user/user" 0 False

      qOwnerRes  <- serveRoutes ownerQuery

      let Right owner = decode . toBytes . Response.queryValue $ qOwnerRes
      owner `shouldBe` expectedOwner



