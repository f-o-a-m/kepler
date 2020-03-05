module Nameservice.Modules.Nameservice.Messages
  ( SetNameMsg(..)
  , BuyNameMsg(..)
  , DeleteNameMsg(..)
  , FaucetAccountMsg(..)
  , BuyNameMessage(..)
  ) where

import           Data.Bifunctor               (bimap, first)
import           Data.Foldable                (sequenceA_)
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           Data.Validation              (Validation (..))
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           Proto3.Suite                 (Message, Named, fromByteString,
                                               toLazyByteString)
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Modules.Auth  (Amount (..), CoinId (..))
import           Tendermint.SDK.Modules.Bank  ()
import           Tendermint.SDK.Types.Address (Address (..))
import           Tendermint.SDK.Types.Message (HasMessageType (..), Msg (..),
                                               ValidateMessage (..),
                                               coerceProto3Error,
                                               formatMessageParseError,
                                               isAuthorCheck, nonEmptyCheck)

-- @NOTE: .proto genration will use these type names as is
-- only field names stripped of prefixes during generation
data FaucetAccountMsg = FaucetAccountMsg
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

instance HasMessageType FaucetAccountMsg where
  messageType _ = "FaucetAccount"

instance HasCodec FaucetAccountMsg where
  encode FaucetAccountMsg {..} =
    let faucetAccountMessaage = FaucetAccountMessage
          { faucetAccountMessageTo = faucetAccountTo
          , faucetAccountMessageCoinId = unCoinId faucetAccountCoinId
          , faucetAccountMessageAmount = unAmount faucetAccountAmount
          }
    in cs . toLazyByteString $ faucetAccountMessaage
  decode =
    let toFaucetAccount FaucetAccountMessage {..} = FaucetAccountMsg
          { faucetAccountTo = faucetAccountMessageTo
          , faucetAccountCoinId = CoinId faucetAccountMessageCoinId
          , faucetAccountAmount = Amount faucetAccountMessageAmount
          }
    in bimap (formatMessageParseError . coerceProto3Error) toFaucetAccount
       . fromByteString @FaucetAccountMessage

instance ValidateMessage FaucetAccountMsg where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------

data SetNameMsg = SetNameMsg
  { setNameName  :: Text
  , setNameOwner :: Address
  , setNameValue :: Text
  } deriving (Eq, Show, Generic)

instance Message SetNameMsg
instance Named SetNameMsg

instance HasMessageType SetNameMsg where
  messageType _ = "SetName"

instance HasCodec SetNameMsg where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

-- TL;DR. ValidateBasic: https://cosmos.network/docs/tutorial/set-name.html#msg
instance ValidateMessage SetNameMsg where
  validateMessage msg@Msg{..} =
    let SetNameMsg{setNameName, setNameValue} = msgData
    in sequenceA_
        [ nonEmptyCheck "Name" setNameName
        , nonEmptyCheck "Value" setNameValue
        , isAuthorCheck "Owner" msg setNameOwner
        ]

--------------------------------------------------------------------------------

data DeleteNameMsg = DeleteNameMsg
  { deleteNameOwner :: Address
  , deleteNameName  :: Text
  } deriving (Eq, Show, Generic)

instance Message DeleteNameMsg
instance Named DeleteNameMsg

instance HasMessageType DeleteNameMsg where
  messageType _ = "DeleteName"

instance HasCodec DeleteNameMsg where
  encode = cs . toLazyByteString
  decode = first (formatMessageParseError . coerceProto3Error) . fromByteString

instance ValidateMessage DeleteNameMsg where
  validateMessage msg@Msg{..} =
    let DeleteNameMsg{deleteNameName} = msgData
    in sequenceA_
       [ nonEmptyCheck "Name" deleteNameName
       , isAuthorCheck "Owner" msg deleteNameOwner
       ]

--------------------------------------------------------------------------------

data BuyNameMsg = BuyNameMsg
  { buyNameBid   :: Amount
  , buyNameName  :: Text
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  } deriving (Eq, Show)

data BuyNameMessage = BuyNameMessage
  { buyNameMessageBid   :: Word64
  , buyNameMessageName  :: Text
  , buyNameMessageValue :: Text
  , buyNameMessageBuyer :: Address
  } deriving (Eq, Show, Generic)
instance Message BuyNameMessage
instance Named BuyNameMessage

instance HasMessageType BuyNameMsg where
  messageType _ = "BuyName"

instance HasCodec BuyNameMsg where
  encode BuyNameMsg {..} =
    let buyNameMessage = BuyNameMessage
          { buyNameMessageBid = unAmount buyNameBid
          , buyNameMessageName = buyNameName
          , buyNameMessageValue = buyNameValue
          , buyNameMessageBuyer = buyNameBuyer
          }
    in cs . toLazyByteString $ buyNameMessage
  decode =
    let toBuyName BuyNameMessage {..} = BuyNameMsg
          { buyNameBid = Amount buyNameMessageBid
          , buyNameName = buyNameMessageName
          , buyNameValue = buyNameMessageValue
          , buyNameBuyer = buyNameMessageBuyer
          }
    in bimap (formatMessageParseError . coerceProto3Error) toBuyName
       . fromByteString @BuyNameMessage

instance ValidateMessage BuyNameMsg where
  validateMessage msg@Msg{..} =
    let BuyNameMsg{buyNameName, buyNameValue} = msgData
    in sequenceA_
        [ nonEmptyCheck "Name" buyNameName
        , nonEmptyCheck "Value" buyNameValue
        , isAuthorCheck "Owner" msg buyNameBuyer
        ]
