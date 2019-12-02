module Nameservice.Modules.TypedMessage where

import qualified Data.ByteString      as BS
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Proto3.Suite         (Message, Named)
import           Tendermint.SDK.Codec (HasCodec (..))

-- Tags messages to disambiguate decoding instances
data TypedMessage = TypedMessage
  { typedMessageType     :: Text
  , typedMessageContents :: BS.ByteString
  } deriving (Eq, Show, Generic)

instance Message TypedMessage
instance Named TypedMessage
instance HasCodec TypedMessage
