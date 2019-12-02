module Nameservice.Modules.Nameservice.Messages where

import           Data.Foldable                         (sequenceA_)
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Nameservice.Modules.Nameservice.Types (Name (..))
import           Nameservice.Modules.Token             (Amount (..))
import           Nameservice.Modules.TypedMessage      (TypedMessage (..))
import           Proto3.Suite                          (Message, Named)
import           Tendermint.SDK.Codec                  (HasCodec (..))
import           Tendermint.SDK.Types.Address          (Address (..))
import           Tendermint.SDK.Types.Message          (Msg (..),
                                                        ValidateMessage (..),
                                                        isAuthorCheck,
                                                        nonEmptyCheck)

data NameserviceMessage =
    NSetName SetName
  | NBuyName BuyName
  | NDeleteName DeleteName
  deriving (Eq, Show, Generic)

-- @NOTE: .proto genration will use these type names as is
-- only field names stripped of prefixes during generation
data SetName = SetName
  { setNameName  :: Name
  , setNameOwner :: Address
  , setNameValue :: Text
  } deriving (Eq, Show, Generic)

instance Message SetName
instance Named SetName
instance HasCodec SetName

data DeleteName = DeleteName
  { deleteNameOwner :: Address
  , deleteNameName  :: Name
  } deriving (Eq, Show, Generic)

instance Message DeleteName
instance Named DeleteName
instance HasCodec DeleteName

data BuyName = BuyName
  { buyNameBid   :: Amount
  , buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  } deriving (Eq, Show, Generic)

instance Message BuyName
instance Named BuyName
instance HasCodec BuyName

instance HasCodec NameserviceMessage where
  decode bs = do
    TypedMessage{..} <- decode bs
    case typedMessageType of
      "SetName" -> NSetName <$> decode typedMessageContents
      "DeleteName" -> NDeleteName <$> decode typedMessageContents
      "BuyName" -> NBuyName <$> decode typedMessageContents
      _ -> Left . cs $ "Unknown Nameservice message type " ++ cs typedMessageType
  encode = \case
    NSetName msg -> encode msg
    NBuyName msg -> encode msg
    NDeleteName msg -> encode msg

instance ValidateMessage NameserviceMessage where
  validateMessage m@Msg{msgData} = case msgData of
    NBuyName msg    -> validateMessage m {msgData = msg}
    NSetName msg    -> validateMessage m {msgData = msg}
    NDeleteName msg -> validateMessage m {msgData = msg}

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
