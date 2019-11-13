module Nameservice.Modules.Nameservice.Messages where

import           Control.Lens                          ((^.))
import qualified Data.Aeson                            as A
import           Data.Either                           (Either)
import qualified Data.ProtoLens                        as PL
import           Data.Proxy
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           GHC.TypeLits                          (symbolVal)
import           Nameservice.Aeson                     (defaultNameserviceOptions)
import           Nameservice.Modules.Nameservice.Types (Name (..),
                                                        NameserviceModule)
import           Nameservice.Modules.Token             (Address (..),
                                                        Amount (..))
import qualified Proto.Nameservice.Messages            as M
import qualified Proto.Nameservice.Messages_Fields     as M
import           Tendermint.SDK.Auth                   (Address,
                                                        MakeMessage (..),
                                                        Msg (..),
                                                        addressFromBytes)

data NameserviceMessage =
    SetName MsgSetName
  | BuyName MsgBuyName
  | DeleteName MsgDeleteName

instance MakeMessage NameserviceMessage where
  makeMessage m = case m of
    SetName msg -> (makeMessage msg) {msgData = m}
    BuyName msg -> (makeMessage msg) {msgData = m}
    DeleteName msg -> (makeMessage msg) {msgData = m}

data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  } deriving Generic

instance A.ToJSON MsgSetName where
  toJSON = A.genericToJSON (defaultNameserviceOptions "msgSetName")

-- TL;DR. ValidateBasic: https://cosmos.network/docs/tutorial/set-name.html#msg
fromProtoMsgSetName :: M.SetName -> Either Text MsgSetName
fromProtoMsgSetName msg = do
  msgName <- nonEmpty "Name" . cs $ msg ^. M.name
  msgValue <- nonEmpty "Value" . cs $ msg ^. M.value
  msgOwner <- nonEmpty "Owner" $ msg ^. M.owner
  return MsgSetName { msgSetNameName = Name msgName
                    , msgSetNameValue = msgValue
                    , msgSetNameOwner = addressFromBytes msgOwner
                    }

instance MakeMessage MsgSetName where
  makeMessage msg@MsgSetName{..} = Msg
    { msgRoute = cs . symbolVal $ (Proxy :: Proxy NameserviceModule)
    , msgType = "SetName"
    , msgSignBytes = cs . A.encode $ msg
    , msgGetSigner = msgSetNameOwner
    , msgValidate = Nothing
    , msgData = msg
    }

data MsgDeleteName = MsgDeleteName
  { msgDeleteNameName  :: Name
  , msgDeleteNameOwner :: Address
  } deriving Generic

instance A.ToJSON MsgDeleteName where
  toJSON = A.genericToJSON (defaultNameserviceOptions "msgDeleteName")

fromProtoMsgDeleteName :: M.DeleteName -> Either Text MsgDeleteName
fromProtoMsgDeleteName msg = do
  msgName <- nonEmpty "Name" . cs $ msg ^. M.name
  msgOwner <- nonEmpty "Owner" $ msg ^. M.owner
  return MsgDeleteName { msgDeleteNameName = Name msgName
                       , msgDeleteNameOwner = addressFromBytes msgOwner
                       }

instance MakeMessage MsgDeleteName where
  makeMessage msg@MsgDeleteName{..} = Msg
    { msgRoute = cs . symbolVal $ (Proxy :: Proxy NameserviceModule)
    , msgType = "DeleteName"
    , msgSignBytes = cs . A.encode $ msg
    , msgGetSigner = msgDeleteNameOwner
    , msgValidate = Nothing
    , msgData = msg
    }

data MsgBuyName = MsgBuyName
    { msgBuyNameName  :: Name
    , msgBuyNameValue :: String
    , msgBuyNameBuyer :: Address
    , msgBuyNameBid   :: Amount
    } deriving Generic

instance A.ToJSON MsgBuyName where
  toJSON = A.genericToJSON (defaultNameserviceOptions "msgBuyName")

fromProtoMsgBuyName :: M.BuyName -> Either Text MsgBuyName
fromProtoMsgBuyName msg = do
  msgName <- nonEmpty "Name" . cs $ msg ^. M.name
  msgValue <- nonEmpty "Value" . cs $ msg ^. M.value
  msgBuyer <- nonEmpty "Buyer"  $ msg ^. M.buyer
  msgBid <- Right . Amount $ msg ^. M.bid
  return MsgBuyName { msgBuyNameName = Name msgName
                    , msgBuyNameValue = msgValue
                    , msgBuyNameBuyer = addressFromBytes msgBuyer
                    , msgBuyNameBid = msgBid
                    }

instance MakeMessage MsgBuyName where
  makeMessage msg@MsgBuyName{..} = Msg
    { msgRoute = cs . symbolVal $ (Proxy :: Proxy NameserviceModule)
    , msgType = "BuyName"
    , msgSignBytes = cs . A.encode $ msg
    , msgGetSigner = msgBuyNameBuyer
    , msgValidate = Nothing
    , msgData = msg
    }

nonEmpty :: (Eq a, Monoid a) => String -> a -> Either Text a
nonEmpty field x | x == mempty = Left . cs $ (show field ++ ": value cannot be empty")
                 | otherwise = Right x