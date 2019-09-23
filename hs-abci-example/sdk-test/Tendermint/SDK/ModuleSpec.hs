
module Tendermint.SDK.ModuleSpec where

import qualified Control.Concurrent.MVar               as MVar
import           Control.Lens                          (to, (^.))
import           Control.Monad                         (void)
import           Control.Monad.Trans
import           Data.ByteArray.HexString              (fromBytes, toBytes)
import           Data.Conduit
import           Data.Proxy
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
      TendermintIO {ioQuery, ioServer, ioSubscribe} <- runApp userComponent ()
      logsVar <- MVar.newMVar []
      charlesLogsVar <- MVar.newMVar []
      let irakli = Buyer { buyerId = "1"
                         , buyerName = "irakli"
                         }
          irakliKey = BuyerKey "1"
          charles = Buyer { buyerId = "2"
                          , buyerName = "charles"
                          }
          logger = awaitForever $ \case
                     StoredBuyer buyer -> lift $ MVar.modifyMVar_ logsVar (pure . (:) buyer)
          charlesLogger = awaitForever $ \case
                            StoredBuyer buyer ->
                              lift $ if buyerName buyer == "charles"
                                then MVar.modifyMVar_ charlesLogsVar (pure . (:) buyer)
                                else pure ()
      _ <- ioSubscribe logger
      _ <- ioSubscribe charlesLogger
      void $ ioQuery $ tell (PutBuyer irakli)
      void $ ioQuery $ tell (PutBuyer charles)
      mIrakli <- ioQuery $ request (GetBuyer irakliKey)
      mIrakli `shouldBe` Just irakli
      mNobody <- ioQuery $ request (GetBuyer (BuyerKey "3"))
      mNobody `shouldBe` Nothing
      logs <- MVar.readMVar logsVar
      logs `shouldBe` [charles, irakli]
      charlesLogs <- MVar.readMVar charlesLogsVar
      charlesLogs `shouldBe` [charles]

      let serveRoutes :: Application IO
          serveRoutes = serve (Proxy :: Proxy UserApi) (Proxy :: Proxy IO) ioServer
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

data UserMessage =
  StoredBuyer Buyer

evalQuery :: forall a action. UserQ a -> TendermintM UserStore action UserMessage IO a
evalQuery (PutBuyer buyer a) = do
  withState $ \store ->
    putBuyer (BuyerKey $ buyerId buyer) buyer store
  raise $ StoredBuyer buyer
  pure a
evalQuery (GetBuyer buyerKey f) = do
  buyer <- withState $
    \store -> get (undefined :: Root) buyerKey store
  pure $ f buyer

type UserApi = "user" :> QueryApi UserStoreContents

userComponentSpec :: ComponentSpec UserStore UserQ action input UserMessage UserApi IO
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

userComponent :: forall (input :: *). Component UserQ input UserMessage UserApi IO
userComponent = Component userComponentSpec
