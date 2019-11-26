module Nameservice.Modules.Token.Messages where

import           Data.Bifunctor                   (first)
import           Data.String.Conversions          (cs)
import           GHC.Generics                     (Generic)
import           Nameservice.Modules.Token.Types  (Amount)
import           Nameservice.Modules.TypedMessage (TypedMessage (..))
import           Proto3.Suite                     (Message, Named,
                                                   fromByteString,
                                                   toLazyByteString)
import           Tendermint.SDK.Codec             (HasCodec (..))
import           Tendermint.SDK.Types.Address     (Address)
import           Tendermint.SDK.Types.Message     (coerceProto3Error,
                                                   formatMessageParseError)

data TokenMessage =
    TTransfer Transfer
  | TFaucetAccount FaucetAccount
  | TBurn Burn
  | TMint Mint
  deriving (Eq, Show, Generic)

data FaucetAccount = FaucetAccount
  { faucetAccountTo     :: Address
  , faucetAccountAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Message FaucetAccount
instance Named FaucetAccount

instance HasCodec FaucetAccount where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data Transfer = Transfer
  { transferTo     :: Address
  , transferFrom   :: Address
  , transferAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Message Transfer
instance Named Transfer

instance HasCodec Transfer where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data Burn = Burn
  { burnAddress :: Address
  , burnAmount  :: Amount
  } deriving (Eq, Show, Generic)

instance Message Burn
instance Named Burn

instance HasCodec Burn where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data Mint = Mint
  { mintAmount  :: Amount
  , mintAddress :: Address
  } deriving (Eq, Show, Generic)

instance Message Mint
instance Named Mint

instance HasCodec Mint where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance HasCodec TokenMessage where
  decode bs = do
    TypedMessage{..} <- decode bs
    case typedMessageType of
      "Transfer" -> TTransfer <$> decode typedMessageContents
      "Burn" -> TBurn <$> decode typedMessageContents
      "Mint" -> TMint <$> decode typedMessageContents
      "FaucetAccount" -> TFaucetAccount <$> decode typedMessageContents
      _ -> Left . cs $ "Unknown Token message type " ++ cs typedMessageType
  encode = \case
    TTransfer msg -> encode msg
    TBurn msg -> encode msg
    TMint msg -> encode msg
    TFaucetAccount msg -> encode msg
