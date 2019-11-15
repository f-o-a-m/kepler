module Nameservice.Modules.Nameservice.Messages where

import           Data.Text                             (Text)
import           GHC.Generics                          (Generic)
import           Nameservice.Modules.Nameservice.Types (Name (..))
import           Nameservice.Modules.Token             (Amount (..))
import           Proto3.Suite                          (Message, Named)
import           Tendermint.SDK.Auth                   (Address)

data NameserviceMessage =
    NSetName SetName
  | NBuyName BuyName
  | NDeleteName DeleteName

-- @NOTE: .proto genration will use these type names as is
-- only field names stripped of prefixes during generation
data SetName = SetName
  { setNameName  :: Name
  , setNameValue :: Text
  , setNameOwner :: Address
  } deriving (Eq, Show, Generic)

instance Message SetName
instance Named SetName

data DeleteName = DeleteName
  { deleteNameName  :: Name
  , deleteNameOwner :: Address
  } deriving (Eq, Show, Generic)

instance Message DeleteName
instance Named DeleteName

data BuyName = BuyName
  { buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  , buyNameBid   :: Amount
  } deriving (Eq, Show, Generic)

instance Message BuyName
instance Named BuyName
