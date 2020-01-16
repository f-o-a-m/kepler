module Tendermint.SDK.Modules.Bank.Messages where

import           Data.Bifunctor                      (first)
import           Data.String.Conversions             (cs)
import           Data.Validation                     (Validation (..))
import           GHC.Generics                        (Generic)
import           Proto3.Suite                        (Message, Named,
                                                      fromByteString,
                                                      toLazyByteString)
import           Tendermint.SDK.Codec                (HasCodec (..))
import           Tendermint.SDK.Modules.Auth         (Amount, CoinId)
import           Tendermint.SDK.Modules.Bank.Types   ()
import           Tendermint.SDK.Modules.TypedMessage (TypedMessage (..))
import           Tendermint.SDK.Types.Address        (Address)
import           Tendermint.SDK.Types.Message        (Msg (..),
                                                      ValidateMessage (..),
                                                      coerceProto3Error,
                                                      formatMessageParseError)

data BankMessage =
    TTransfer Transfer
  | TFaucetAccount FaucetAccount
  | TBurn Burn
  deriving (Eq, Show, Generic)

data FaucetAccount = FaucetAccount
  { faucetAccountTo     :: Address
  , faucetAccountCoinId :: CoinId
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
  , transferCoinId :: CoinId
  , transferAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Message Transfer
instance Named Transfer

instance HasCodec Transfer where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data Burn = Burn
  { burnAddress :: Address
  , burnCoinId  :: CoinId
  , burnAmount  :: Amount
  } deriving (Eq, Show, Generic)

instance Message Burn
instance Named Burn

instance HasCodec Burn where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance HasCodec BankMessage where
  decode bs = do
    TypedMessage{..} <- decode bs
    case typedMessageType of
      "Transfer" -> TTransfer <$> decode typedMessageContents
      "Burn" -> TBurn <$> decode typedMessageContents
      "FaucetAccount" -> TFaucetAccount <$> decode typedMessageContents
      _ -> Left . cs $ "Unknown Bank message type " ++ cs typedMessageType
  encode = \case
    TTransfer msg -> encode msg
    TBurn msg -> encode msg
    TFaucetAccount msg -> encode msg

instance ValidateMessage BankMessage where
  validateMessage m@Msg{msgData} = case msgData of
    TTransfer msg      -> validateMessage m {msgData = msg}
    TFaucetAccount msg -> validateMessage m {msgData = msg}
    TBurn msg          -> validateMessage m {msgData = msg}

instance ValidateMessage Transfer where
  validateMessage _ = Success ()

instance ValidateMessage FaucetAccount where
  validateMessage _ = Success ()

instance ValidateMessage Burn where
  validateMessage _ = Success ()
