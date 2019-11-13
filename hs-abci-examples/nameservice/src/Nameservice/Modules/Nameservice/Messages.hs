module Nameservice.Modules.Nameservice.Messages where

import qualified Data.Aeson                            as A
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Nameservice.Aeson                     (defaultNameserviceOptions)
import           Nameservice.Modules.Nameservice.Types (Name)
import           Nameservice.Modules.Token             (Address, Amount)
import           Proto.Nameservice.Messages            ()
import           Proto.Nameservice.Messages_Fields     ()
import           Tendermint.SDK.Auth                   (IsTxMessage (..),
                                                        Msg (..))

data NameserviceMessage =
    SetName MsgSetName
  | BuyName MsgBuyName
  | DeleteName MsgDeleteName

moduleName :: Text
moduleName = "Nameservice"

data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  } deriving Generic

instance A.ToJSON MsgSetName where
  toJSON = A.genericToJSON  (defaultNameserviceOptions "msgSetName")

instance A.FromJSON MsgSetName where
  parseJSON = A.genericParseJSON  (defaultNameserviceOptions "msgSetName")

instance IsTxMessage MsgSetName where
  makeTxMessage msg@MsgSetName{..} = Msg
    { msgRoute = moduleName
    , msgType = "SetName"
    , msgSignBytes = cs $ A.encode msg
    , msgGetSigners = msgSetNameOwner
    , msgValidate = Nothing
    , msgData = msg
    }

data MsgDeleteName = MsgDeleteName
  { msgDeleteNameName  :: Name
  , msgDeleteNameOwner :: Address
  } deriving Generic

instance A.ToJSON MsgDeleteName where
  toJSON = A.genericToJSON  (defaultNameserviceOptions "msgDeleteName")

instance A.FromJSON MsgDeleteName where
  parseJSON = A.genericParseJSON  (defaultNameserviceOptions "msgDeleteName")

instance IsTxMessage MsgDeleteName where
  makeTxMessage msg@MsgDeleteName{..} = Msg
    { msgRoute = moduleName
    , msgType = "DeleteName"
    , msgSignBytes = cs $ A.encode msg
    , msgGetSigners = msgDeleteNameOwner
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
  toJSON = A.genericToJSON  (defaultNameserviceOptions "msgBuyName")

instance A.FromJSON MsgBuyName where
  parseJSON = A.genericParseJSON  (defaultNameserviceOptions "msgBuyName")

instance IsTxMessage MsgBuyName where
  makeTxMessage msg@MsgBuyName{..} = Msg
    { msgRoute = moduleName
    , msgType = "BuyName"
    , msgSignBytes = cs $ A.encode msg
    , msgGetSigners = msgBuyNameBuyer
    , msgValidate = Nothing
    , msgData = msg
    }
