module Tendermint.SDK.Test.StoreExampleSpec where

import           Control.Lens                         (to, (^.))
import           Data.ByteArray.Base64String          (fromBytes, toBytes)
import           Data.Proxy
import qualified Network.ABCI.Types.Messages.Request  as Request
import qualified Network.ABCI.Types.Messages.Response as Response
import           Servant.API
import           Tendermint.SDK.AuthTreeStore
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Router
import           Tendermint.SDK.Store
import           Tendermint.SDK.StoreQueries
import           Tendermint.SDK.Test.StoreExample
import           Test.Hspec
import           Test.QuickCheck.Arbitrary            (arbitrary)
import           Test.QuickCheck.Gen                  (generate)

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

      -- | Owner
      ownerKey <- generate arbitrary
      expectedOwner <- generate arbitrary
      putOwner ownerKey expectedOwner userStore
      let ownerKeyHex = ownerKey ^. rawKey . to fromBytes
          ownerQuery = Request.Query ownerKeyHex "user/owner" 0 False
      qOwnerRes  <- serveRoutes ownerQuery
      let Right owner = decode . toBytes . Response.queryValue $ qOwnerRes
      owner `shouldBe` expectedOwner

      -- | Buyer
      buyerKey <- generate arbitrary
      expectedBuyer <- generate arbitrary
      putBuyer buyerKey expectedBuyer userStore
      let buyerKeyHex = buyerKey ^. rawKey . to fromBytes
          buyerQuery = Request.Query buyerKeyHex "user/buyer" 0 False
      qBuyerRes  <- serveRoutes buyerQuery
      let Right buyer = decode . toBytes . Response.queryValue $ qBuyerRes
      buyer `shouldBe` expectedBuyer

      -- | Lab
      labKey <- generate arbitrary
      expectedLab <- generate arbitrary
      putLab labKey expectedLab dogStore
      let labKeyHex = labKey ^. rawKey . to fromBytes
          labQuery = Request.Query labKeyHex "dog/lab" 0 False
      qLabRes  <- serveRoutes labQuery
      let Right lab = decode . toBytes . Response.queryValue $ qLabRes
      lab `shouldBe` expectedLab

      -- | Hound
      houndKey <- generate arbitrary
      expectedHound <- generate arbitrary
      putHound houndKey expectedHound dogStore
      let houndKeyHex = houndKey ^. rawKey . to fromBytes
          houndQuery = Request.Query houndKeyHex "dog/hound" 0 False
      qHoundRes  <- serveRoutes houndQuery
      let Right hound = decode . toBytes . Response.queryValue $ qHoundRes
      hound `shouldBe` expectedHound

