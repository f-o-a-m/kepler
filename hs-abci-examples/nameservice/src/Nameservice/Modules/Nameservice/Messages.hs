module Nameservice.Modules.Nameservice.Messages where

import           Control.Lens                          ((^.))
import           Data.Either                           (Either)
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           Data.Text                             as T
import           Nameservice.Modules.Nameservice.Types (Name (..))
import           Nameservice.Modules.Token             (Address (..), Amount)
import           Proto.Nameservice.Messages            as M
import           Proto.Nameservice.Messages_Fields     as M

data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  }

-- TL;DR. ValidateBasic: https://cosmos.network/docs/tutorial/set-name.html#msg
fromProtoMsgSetName :: M.SetName -> Either Text MsgSetName
fromProtoMsgSetName msg = do
  msgName <- nonEmpty . T.unpack $ msg ^. M.name
  msgValue <- nonEmpty . T.unpack $ msg ^. M.value
  msgOwner <- nonEmpty . cs $ msg ^. M.owner
  return MsgSetName { msgSetNameName = Name msgName
                    , msgSetNameValue = msgValue
                    , msgSetNameOwner = Address msgOwner
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

fromProtoMsgBuyName :: M.BuyName -> Either Text MsgBuyName
fromProtoMsgBuyName = undefined

nonEmpty :: String -> Either Text String
nonEmpty str | str == "" = Left . T.pack $ "name cannot be empty"
             | otherwise = Right $ str
