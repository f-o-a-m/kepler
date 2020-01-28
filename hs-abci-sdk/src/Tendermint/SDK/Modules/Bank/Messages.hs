module Tendermint.SDK.Modules.Bank.Messages where

import           Data.Bifunctor                    (first)
import           Data.String.Conversions           (cs)
import           Data.Validation                   (Validation (..))
import           GHC.Generics                      (Generic)
import           Proto3.Suite                      (Message, Named,
                                                    fromByteString,
                                                    toLazyByteString)
import           Tendermint.SDK.Codec              (HasCodec (..))
import           Tendermint.SDK.Modules.Auth       (Amount, CoinId)
import           Tendermint.SDK.Modules.Bank.Types ()
import           Tendermint.SDK.Types.Address      (Address)
import           Tendermint.SDK.Types.Message      (HasMessageType (..),
                                                    ValidateMessage (..),
                                                    coerceProto3Error,
                                                    formatMessageParseError)

data Transfer = Transfer
  { transferTo     :: Address
  , transferFrom   :: Address
  , transferCoinId :: CoinId
  , transferAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Message Transfer
instance Named Transfer

instance HasMessageType Transfer where
  messageType _ = "Transfer"

instance HasCodec Transfer where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance ValidateMessage Transfer where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------

data Burn = Burn
  { burnAddress :: Address
  , burnCoinId  :: CoinId
  , burnAmount  :: Amount
  } deriving (Eq, Show, Generic)

instance Message Burn
instance Named Burn

instance HasMessageType Burn where
  messageType _ = "Burn"

instance HasCodec Burn where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance ValidateMessage Burn where
  validateMessage _ = Success ()
