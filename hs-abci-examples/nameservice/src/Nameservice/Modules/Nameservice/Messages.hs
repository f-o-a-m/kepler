module Nameservice.Modules.Nameservice.Messages where

import           Nameservice.Modules.Nameservice.Types (Name)
import           Nameservice.Modules.Token             (Address, Amount)
import Proto.Nameservice.Messages ()
import Proto.Nameservice.Messages_Fields ()

data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  }

data MsgDeleteName = MsgDeleteName
  { msgDeleteNameName  :: Name
  , msgDeleteNameOwner :: Address
  }

data MsgBuyName = MsgBuyName
    { msgBuyNameName  :: Name
    , msgBuyNameValue :: String
    , msgBuyNameBuyer :: Address
    , msgBuyNameBid   :: Amount
    }
