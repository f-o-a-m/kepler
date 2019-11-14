module Nameservice.Modules.Nameservice.Messages where

import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Nameservice.Modules.Nameservice.Types (Name (..))
import           Nameservice.Modules.Token             (Amount (..))
import           Proto3.Suite                          (Message, Named)
import           Tendermint.SDK.Auth                   (Address)

data NameserviceMessage =
    SetName MsgSetName
  | BuyName MsgBuyName
  | DeleteName MsgDeleteName

data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: Text
  , msgSetNameOwner :: Address
  } deriving (Eq, Show, Generic)

instance Message MsgSetName
instance Named MsgSetName

data MsgDeleteName = MsgDeleteName
  { msgDeleteNameName  :: Name
  , msgDeleteNameOwner :: Address
  } deriving (Eq, Show, Generic)

instance Message MsgBuyName
instance Named MsgBuyName

data MsgBuyName = MsgBuyName
  { msgBuyNameName  :: Name
  , msgBuyNameValue :: Text
  , msgBuyNameBuyer :: Address
  , msgBuyNameBid   :: Amount
  } deriving (Eq, Show, Generic)

instance Message MsgDeleteName
instance Named MsgDeleteName
