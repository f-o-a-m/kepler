
module Tendermint.SDK.ModuleSpec where

import           Control.Lens                          (to, (^.))
import           Control.Monad                         (void)
import           Data.ByteArray.HexString              (fromBytes, toBytes)
import qualified Network.ABCI.Types.Messages.Request   as Request
import qualified Network.ABCI.Types.Messages.Response  as Response
import           Servant.API                           ((:>))
import           Tendermint.SDK.Codec
import           Tendermint.SDK.Module
import           Tendermint.SDK.Router
import           Tendermint.SDK.Store
import           Tendermint.SDK.StoreExample
import           Tendermint.SDK.StoreExample.Instances ()
import           Tendermint.SDK.StoreQueries
import           Test.Hspec

spec :: Spec
spec =
  describe "UserModule" $ do
    it "can create the user module and query it via Query msg and from component" $ do
      TendermintIO {ioQuery, ioRouter} <- runTendermint userComponent ()
      let irakli = Buyer { buyerId = "1"
                         , buyerName = "irakli"
                         }
          irakliKey = BuyerKey "1"
      void $ ioQuery $ tell (PutBuyer irakli)
      mIrakli <- ioQuery $ request (GetBuyer irakliKey)
      mIrakli `shouldBe` Just irakli
      mNobody <- ioQuery $ request (GetBuyer (BuyerKey "2"))
      mNobody `shouldBe` Nothing

      let serveRoutes :: Application IO
          serveRoutes = serveRouter ioRouter
          irakliKeyHex = irakliKey ^. rawKey . to fromBytes
          irakliQuery = Request.Query irakliKeyHex "user/buyer" 0 False
      qBuyerRes  <- serveRoutes irakliQuery
      let ebuyer = decode . toBytes . Response.queryValue $ qBuyerRes
      ebuyer `shouldBe` Right irakli

--------------------------------------------------------------------------------
-- User Module
--------------------------------------------------------------------------------

data UserQ a =
    PutBuyer Buyer a
  | GetBuyer BuyerKey (Maybe Buyer -> a)

evalQuery :: forall a action. UserQ a -> TendermintM UserStore action IO a
evalQuery (PutBuyer buyer a) = do
  withState $ \store ->
    putBuyer (BuyerKey $ buyerId buyer) buyer store
  pure a
evalQuery (GetBuyer buyerKey f) = do
  buyer <- withState $
    \store -> get (undefined :: Root) buyerKey store
  pure $ f buyer

type UserApi = "user" :> QueryApi UserStoreContents

userComponentSpec :: ComponentSpec UserStore UserQ action input UserApi IO
userComponentSpec = ComponentSpec
  { initialState = const $ do
      rawStore <- mkAuthTreeStore
      pure $ Store
        { storeRawStore = rawStore }
  , eval = evaluator
  , mkServer = userServer
  }
  where
    userServer :: UserStore -> RouteT UserApi IO
    userServer = allStoreHandlers

    evaluator = mkEval $ EvalSpec
      { handleAction = const $ pure ()
      , handleQuery = evalQuery
      , receive = const Nothing
      , initialize = Nothing
      }

userComponent :: forall (input :: *). Component UserQ input UserApi IO
userComponent = Component userComponentSpec
