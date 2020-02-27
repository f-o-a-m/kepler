module Tendermint.Utils.Test.EventSpec (spec) where

import qualified Data.Aeson                    as A
import           Data.Bifunctor                (first)
import           Data.String.Conversions       (cs)
import           Data.Text                     (pack)
import           GHC.Generics                  (Generic)
import           Tendermint.SDK.BaseApp.Events (ToEvent (..))
import           Tendermint.SDK.Codec          (HasCodec (..))
import           Tendermint.Utils.Events       (FromEvent (..))
import           Test.Hspec

spec :: Spec
spec = describe "Event Tests" $ do
    it "Can serialize and deserialize and event" $ do
      let transferEv =  Transfer
            { to = "me"
            , from = "you"
            , amount = 1
            }
      fromEvent (toEvent transferEv) `shouldBe` Right transferEv


newtype WrappedInt = WrappedInt {unwrapInt :: Int}
  deriving (Eq, Show, Generic, A.ToJSON, A.FromJSON, Num)

instance HasCodec WrappedInt where
  encode (WrappedInt i) = cs $ A.encode i
  decode = first pack . A.eitherDecodeStrict

data Transfer = Transfer
  { to     :: String
  , from   :: String
  , amount :: WrappedInt
  } deriving (Eq, Show, Generic)


instance A.ToJSON Transfer
instance A.FromJSON Transfer
instance ToEvent Transfer
instance FromEvent Transfer
