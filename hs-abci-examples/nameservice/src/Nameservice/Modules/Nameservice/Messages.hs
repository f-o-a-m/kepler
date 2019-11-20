module Nameservice.Modules.Nameservice.Messages where

import           Control.Applicative                   ((<|>))
import           Control.Lens                          (( # ), (^.))
import qualified Data.Aeson                            as A
import           Data.Bifunctor                        (first)
import           Data.Either                           (Either)
import           Data.Foldable                         (sequenceA_)
import           Data.Proxy
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import qualified Data.Validation                       as V
import           GHC.Generics                          (Generic)
import           GHC.TypeLits                          (symbolVal)
import           Nameservice.Aeson                     (defaultNameserviceOptions)
import           Nameservice.Modules.Nameservice.Types (Name (..),
                                                        NameserviceModule)
import           Nameservice.Modules.Token             (Amount (..))
import           Proto3.Suite                          (Message, Named,
                                                        fromByteString,
                                                        toLazyByteString)
import           Tendermint.SDK.Codec                  (HasCodec (..))
import           Tendermint.SDK.Types.Address          (Address,
                                                        addressFromBytes)
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
  deriving (Eq, Show, Generic)

-- @NOTE: .proto genration will use these type names as is
-- only field names stripped of prefixes during generation
data SetName = SetName
  { setNameName  :: Name
  , setNameValue :: Text
  , setNameOwner :: Address
  } deriving (Eq, Show, Generic)

instance Message SetName
instance Named SetName

instance HasCodec SetName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data DeleteName = DeleteName
  { deleteNameName  :: Name
  , deleteNameOwner :: Address
  } deriving (Eq, Show, Generic)

instance Message DeleteName
instance Named DeleteName

instance HasCodec DeleteName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

data BuyName = BuyName
  { buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  , buyNameBid   :: Amount
  } deriving (Eq, Show, Generic)

instance Message BuyName
instance Named BuyName

instance HasCodec BuyName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance HasCodec NameserviceMessage where
  decode bs =
    fmap NSetName (decode bs) <>
    fmap NBuyName (decode bs) <>
    fmap NDeleteName (decode bs)
  encode = \case
    NSetName msg -> encode msg
    NBuyName msg -> encode msg
    NDeleteName msg -> encode msg

instance ValidateMessage NameserviceMessage where
  validateMessage m@Msg{msgData} = case msgData of
    NSetName msg    -> validateMessage m {msgData = msg}
    NBuyName msg    -> validateMessage m {msgData = msg}
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
