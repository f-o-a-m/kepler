module Nameservice.Modules.Token.Messages
  ( Transfer(..)
  , Burn(..)
  , FaucetAccount(..)
  ) where

import           Data.Bifunctor                  (first)
import           Data.String.Conversions         (cs)
import           Data.Validation                 (Validation (..))
import           GHC.Generics                    (Generic)
import           Nameservice.Modules.Token.Types (Amount)
import           Proto3.Suite                    (Message, Named,
                                                  fromByteString,
                                                  toLazyByteString)
import           Tendermint.SDK.Codec            (HasCodec (..))
import           Tendermint.SDK.Types.Address    (Address)
import           Tendermint.SDK.Types.Message    (ValidateMessage (..),
                                                  coerceProto3Error,
                                                  formatMessageParseError)

data FaucetAccount = FaucetAccount
  { faucetAccountTo     :: Address
  , faucetAccountAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Message FaucetAccount
instance Named FaucetAccount

instance HasCodec FaucetAccount where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance ValidateMessage FaucetAccount where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------

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

instance ValidateMessage Transfer where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------

data Burn = Burn
  { burnAddress :: Address
  , burnAmount  :: Amount
  } deriving (Eq, Show, Generic)

instance Message Burn
instance Named Burn

instance HasCodec Burn where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance ValidateMessage Burn where
  validateMessage _ = Success ()
