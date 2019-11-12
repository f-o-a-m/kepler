module Nameservice.Modules.Nameservice.Messages where

import           Control.Lens                          ((^.))
import           Data.Either                           (Either)
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           Nameservice.Modules.Nameservice.Types (Name (..))
import           Nameservice.Modules.Token             (Address (..), Amount(..))
import           Proto.Nameservice.Messages            as M
import           Proto.Nameservice.Messages_Fields     as M

data NameserviceMessage =
    SetName MsgSetName
  | BuyName MsgBuyName
  | DeleteName MsgDeleteName


data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  }

-- TL;DR. ValidateBasic: https://cosmos.network/docs/tutorial/set-name.html#msg
fromProtoMsgSetName :: M.SetName -> Either Text MsgSetName
fromProtoMsgSetName msg = do
  msgName <- nonEmpty "Name" . cs $ msg ^. M.name
  msgValue <- nonEmpty "Value" . cs $ msg ^. M.value
  msgOwner <- nonEmpty "Owner" . cs $ msg ^. M.owner
  return MsgSetName { msgSetNameName = Name msgName
                    , msgSetNameValue = msgValue
                    , msgSetNameOwner = Address msgOwner
                    }

data MsgDeleteName = MsgDeleteName
  { msgDeleteNameName  :: Name
  , msgDeleteNameOwner :: Address
  }

fromProtoMsgDeleteName :: M.DeleteName -> Either Text MsgDeleteName
fromProtoMsgDeleteName msg = do
  msgName <- nonEmpty "Name" . cs $ msg ^. M.name
  msgOwner <- nonEmpty "Owner" . cs $ msg ^. M.owner
  return MsgDeleteName { msgDeleteNameName = Name msgName
                       , msgDeleteNameOwner = Address msgOwner
                       }

data MsgBuyName = MsgBuyName
    { msgBuyNameName  :: Name
    , msgBuyNameValue :: String
    , msgBuyNameBuyer :: Address
    , msgBuyNameBid   :: Amount
    }

fromProtoMsgBuyName :: M.BuyName -> Either Text MsgBuyName
fromProtoMsgBuyName msg = do
  msgName <- nonEmpty "Name" . cs $ msg ^. M.name
  msgValue <- nonEmpty "Value" . cs $ msg ^. M.value
  msgBuyer <- nonEmpty "Buyer" . cs $ msg ^. M.buyer
  msgBid <- Right . Amount $ msg ^. M.bid
  return MsgBuyName { msgBuyNameName = Name msgName
                    , msgBuyNameValue = msgValue
                    , msgBuyNameBuyer = Address msgBuyer
                    , msgBuyNameBid = msgBid
                    }

nonEmpty :: (Eq a, Monoid a) => String -> a -> Either Text a
nonEmpty field x | x == mempty = Left . cs $ (show field ++ ": value cannot be empty")
                 | otherwise = Right x
