module Nameservice.Modules.Messages where

import           Nameservice.Modules.Nameservice (Name)
import           Nameservice.Modules.Token       (Address, Amount)

class HasMsg a where
  msgType :: a -> String
  msgRoute :: a -> String
  -- msgValidateBasic :: a -> _ -- some sort of error
  -- msgGetSignBytes :: _
  msgGetSigners :: a -> [Address]

data MsgSetName = MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  }

instance HasMsg MsgSetName where
  msgType _ = "set_name"
  msgRoute _ = "nameservice"
  -- msgValidateBasic MsgSetName{..} = undefined -- checks for empty fields? (lol)
  -- msgGetSignBytes MsgSetName{..} = undefined -- defines msg encoding
  msgGetSigners MsgSetName{msgSetNameOwner} = [msgSetNameOwner]

data MsgBuyName = MsgBuyName
  { msgBuyNameName  :: Name
  , msgBuyNameBig   :: Amount
  , msgBuyNameBuyer :: Address
  }

instance HasMsg MsgBuyName where
  msgType _ = "buy_name"
  msgRoute _ = "nameservice"
  -- msgValidateBasic MsgBuyName{..} = undefined -- checks for empty fields, positive bid
  -- msgGetSignBytes MsgBuyName{..} = undefined -- defines msg encoding
  msgGetSigners MsgBuyName{msgBuyNameBuyer} = [msgBuyNameBuyer]
