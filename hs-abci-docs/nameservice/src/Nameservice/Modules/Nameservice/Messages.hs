module Nameservice.Modules.Nameservice.Messages
  ( SetName(..)
  , BuyName(..)
  , BuyNameMessage(..)
  , DeleteName(..)
  , FaucetAccount(..)
  ) where

import           Data.Bifunctor                        (bimap, first)
import           Data.Foldable                         (sequenceA_)
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           Data.Validation                       (Validation (..))
import           Data.Word                             (Word64)
import           GHC.Generics                          (Generic)
import           Nameservice.Modules.Nameservice.Types (Name (..))
import           Proto3.Suite                          (Message, Named,
                                                        fromByteString,
                                                        toLazyByteString)
import           Tendermint.SDK.Codec                  (HasCodec (..))
import           Tendermint.SDK.Modules.Auth           (Amount (..),
                                                        CoinId (..))
import           Tendermint.SDK.Modules.Bank           ()
import           Tendermint.SDK.Types.Address          (Address (..))
import           Tendermint.SDK.Types.Message          (HasMessageType (..),
                                                        Msg (..),
                                                        ValidateMessage (..),
                                                        coerceProto3Error,
                                                        formatMessageParseError,
                                                        isAuthorCheck,
                                                        nonEmptyCheck)

-- @NOTE: .proto genration will use these type names as is
-- only field names stripped of prefixes during generation
data FaucetAccount = FaucetAccount
  { faucetAccountTo     :: Address
  , faucetAccountCoinId :: CoinId
  , faucetAccountAmount :: Amount
  } deriving (Eq, Show)

data FaucetAccountMessage = FaucetAccountMessage
  { faucetAccountMessageTo     :: Address
  , faucetAccountMessageCoinId :: Text
  , faucetAccountMessageAmount :: Word64
  } deriving (Eq, Show, Generic)
instance Message FaucetAccountMessage
instance Named FaucetAccountMessage

instance HasMessageType FaucetAccount where
  messageType _ = "FaucetAccount"

instance HasCodec FaucetAccount where
  encode FaucetAccount {..} =
    let faucetAccountMessaage = FaucetAccountMessage
          { faucetAccountMessageTo = faucetAccountTo
          , faucetAccountMessageCoinId = unCoinId faucetAccountCoinId
          , faucetAccountMessageAmount = unAmount faucetAccountAmount
          }
    in cs . toLazyByteString $ faucetAccountMessaage
  decode =
    let toFaucetAccount FaucetAccountMessage {..} = FaucetAccount
          { faucetAccountTo = faucetAccountMessageTo
          , faucetAccountCoinId = CoinId faucetAccountMessageCoinId
          , faucetAccountAmount = Amount faucetAccountMessageAmount
          }
    in bimap (formatMessageParseError . coerceProto3Error) toFaucetAccount
       . fromByteString @FaucetAccountMessage

instance ValidateMessage FaucetAccount where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------

data SetName = SetName
  { setNameName  :: Name
  , setNameOwner :: Address
  , setNameValue :: Text
  } deriving (Eq, Show, Generic)

instance Message SetName
instance Named SetName

instance HasMessageType SetName where
  messageType _ = "SetName"

instance HasCodec SetName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

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

--------------------------------------------------------------------------------

data DeleteName = DeleteName
  { deleteNameOwner :: Address
  , deleteNameName  :: Name
  } deriving (Eq, Show, Generic)

instance Message DeleteName
instance Named DeleteName

instance HasMessageType DeleteName where
  messageType _ = "DeleteName"

instance HasCodec DeleteName where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance ValidateMessage DeleteName where
  validateMessage msg@Msg{..} =
    let DeleteName{deleteNameName} = msgData
        Name name = deleteNameName
    in sequenceA_
       [ nonEmptyCheck "Name" name
       , isAuthorCheck "Owner" msg deleteNameOwner
       ]

--------------------------------------------------------------------------------

data BuyName = BuyName
  { buyNameBid   :: Amount
  , buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  } deriving (Eq, Show)

data BuyNameMessage = BuyNameMessage
  { buyNameMessageBid   :: Word64
  , buyNameMessageName  :: Name
  , buyNameMessageValue :: Text
  , buyNameMessageBuyer :: Address
  } deriving (Eq, Show, Generic)
instance Message BuyNameMessage
instance Named BuyNameMessage

instance HasMessageType BuyName where
  messageType _ = "BuyName"

instance HasCodec BuyName where
  encode BuyName {..} =
    let buyNameMessage = BuyNameMessage
          { buyNameMessageBid = unAmount buyNameBid
          , buyNameMessageName = buyNameName
          , buyNameMessageValue = buyNameValue
          , buyNameMessageBuyer = buyNameBuyer
          }
    in cs . toLazyByteString $ buyNameMessage
  decode =
    let toBuyName BuyNameMessage {..} = BuyName
          { buyNameBid = Amount buyNameMessageBid
          , buyNameName = buyNameMessageName
          , buyNameValue = buyNameMessageValue
          , buyNameBuyer = buyNameMessageBuyer
          }
    in bimap (formatMessageParseError . coerceProto3Error) toBuyName
       . fromByteString @BuyNameMessage

instance ValidateMessage BuyName where
  validateMessage msg@Msg{..} =
    let BuyName{buyNameName, buyNameValue} = msgData
        Name name = buyNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" buyNameValue
        , isAuthorCheck "Owner" msg buyNameBuyer
        ]
