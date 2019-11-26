module Nameservice.Modules.TypedMessage where

import           Data.Bifunctor               (first)
import qualified Data.ByteString              as BS
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import           Proto3.Suite                 (Message, Named, fromByteString,
                                               toLazyByteString)
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Types.Message (coerceProto3Error,
                                               formatMessageParseError)

-- Tags messages to disambiguate decoding instances
data TypedMessage = TypedMessage
  { typedMessageType     :: Text
  , typedMessageContents :: BS.ByteString
  } deriving (Eq, Show, Generic)

instance Message TypedMessage
instance Named TypedMessage

instance HasCodec TypedMessage where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString
