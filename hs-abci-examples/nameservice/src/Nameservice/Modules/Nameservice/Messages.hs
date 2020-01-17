module Nameservice.Modules.Nameservice.Messages where

import           Data.Bifunctor                        (first)
import           Data.Foldable                         (sequenceA_)
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           Data.Validation                       (Validation (..))
import           GHC.Generics                          (Generic)
import           Nameservice.Modules.Nameservice.Types (Name (..))
import           Proto3.Suite                          (Message, Named,
                                                        fromByteString,
                                                        toLazyByteString)
import           Tendermint.SDK.Codec                  (HasCodec (..))
import           Tendermint.SDK.Modules.Auth           (Amount (..),
                                                        CoinId (..))
import           Tendermint.SDK.Modules.Bank           ()
import           Tendermint.SDK.Modules.TypedMessage   (TypedMessage (..))
import           Tendermint.SDK.Types.Address          (Address (..))
import           Tendermint.SDK.Types.Message          (Msg (..),
                                                        ValidateMessage (..),
                                                        coerceProto3Error,
                                                        formatMessageParseError,
                                                        isAuthorCheck,
                                                        nonEmptyCheck)

data NameserviceMessage =
    NSetName SetName
  | NBuyName BuyName
  | NDeleteName DeleteName
  | NFaucetAccount FaucetAccount
  deriving (Eq, Show, Generic)

-- @NOTE: .proto genration will use these type names as is
-- only field names stripped of prefixes during generation
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

data SetName = SetName
  { setNameName  :: Name
  , setNameOwner :: Address
  , setNameValue :: Text
  } deriving (Eq, Show, Generic)

instance Message SetName
instance Named SetName

instance HasCodec SetName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data DeleteName = DeleteName
  { deleteNameOwner :: Address
  , deleteNameName  :: Name
  } deriving (Eq, Show, Generic)

instance Message DeleteName
instance Named DeleteName

instance HasCodec DeleteName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data BuyName = BuyName
  { buyNameBid   :: Amount
  , buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  } deriving (Eq, Show, Generic)

instance Message BuyName
instance Named BuyName

instance HasCodec BuyName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance HasCodec NameserviceMessage where
  decode bs = do
    TypedMessage{..} <- decode bs
    case typedMessageType of
      "SetName" -> NSetName <$> decode typedMessageContents
      "DeleteName" -> NDeleteName <$> decode typedMessageContents
      "BuyName" -> NBuyName <$> decode typedMessageContents
      "FaucetAccount" -> NFaucetAccount <$> decode typedMessageContents
      _ -> Left . cs $ "Unknown Nameservice message type " ++ cs typedMessageType
  encode = \case
    NSetName msg -> encode msg
    NBuyName msg -> encode msg
    NDeleteName msg -> encode msg
    NFaucetAccount msg -> encode msg

instance ValidateMessage NameserviceMessage where
  validateMessage m@Msg{msgData} = case msgData of
    NBuyName msg       -> validateMessage m {msgData = msg}
    NSetName msg       -> validateMessage m {msgData = msg}
    NDeleteName msg    -> validateMessage m {msgData = msg}
    NFaucetAccount msg -> validateMessage m {msgData = msg}

-- TL;DR. ValidateBasic: https://cosmos.network/docs/tutorial/set-name.html#msg
instance ValidateMessage SetName where
  validateMessage msg@Msg{..} =
    let SetName{setNameName, setNameValue} = msgData
        Name name = setNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" setNameValue
        , isAuthorCheck "Owner" msg setNameOwner
        ]

instance ValidateMessage DeleteName where
  validateMessage msg@Msg{..} =
    let DeleteName{deleteNameName} = msgData
        Name name = deleteNameName
    in sequenceA_
       [ nonEmptyCheck "Name" name
       , isAuthorCheck "Owner" msg deleteNameOwner
       ]

instance ValidateMessage BuyName where
  validateMessage msg@Msg{..} =
    let BuyName{buyNameName, buyNameValue} = msgData
        Name name = buyNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" buyNameValue
        , isAuthorCheck "Owner" msg buyNameBuyer
        ]

instance ValidateMessage FaucetAccount where
  validateMessage _ = Success ()
