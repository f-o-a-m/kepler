
module Tendermint.SDK.ModuleSpec where

import           Control.Monad                         (void)
import           Tendermint.SDK.Module
import           Tendermint.SDK.Store
import           Tendermint.SDK.StoreExample
import           Tendermint.SDK.StoreExample.Instances ()
import           Test.Hspec

spec :: Spec
spec =
  describe "UserModule" $ do
    it "can create the user module and query it" $ do
      TendermintIO {query} <- runTendermint userComponent ()
      let irakli = Buyer { buyerId = "1"
                         , buyerName = "irakli"
                         }
      void $ query $ tell (PutBuyer irakli)
      mIrakli <- query $ request (GetBuyer (BuyerKey "1"))
      mIrakli `shouldBe` Just irakli
      mNobody <- query $ request (GetBuyer (BuyerKey "2"))
      mNobody `shouldBe` Nothing

--------------------------------------------------------------------------------
-- User Module
--------------------------------------------------------------------------------

data UserQ a =
    PutBuyer Buyer a
  | GetBuyer BuyerKey (Maybe Buyer -> a)

evalQuery :: forall a action. UserQ a -> TendermintM UserStore action IO a
evalQuery (PutBuyer buyer a) = do
  tState $ \store ->
    putBuyer (BuyerKey $ buyerId buyer) buyer store
  pure a
evalQuery (GetBuyer buyerKey f) = do
  buyer <- tState $
    \store -> get (undefined :: Root) buyerKey store
  pure $ f buyer

userComponentSpec :: ComponentSpec UserStore UserQ action input IO
userComponentSpec = ComponentSpec
  { initialState = const $ do
      rawStore <- mkAuthTreeStore
      pure $ Store
        { storeRawStore = rawStore }
  , eval = evaluator
  }
  where
    evaluator = mkEval $ EvalSpec
      { handleAction = const $ pure ()
      , handleQuery = evalQuery
      , receive = const Nothing
      , initialize = Nothing
      }

userComponent :: forall (input :: *). Component UserQ input IO
userComponent = mkComponent userComponentSpec
