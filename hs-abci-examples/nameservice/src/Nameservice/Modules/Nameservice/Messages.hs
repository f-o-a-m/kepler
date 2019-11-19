module Nameservice.Modules.Nameservice.Messages where

import           Control.Applicative                   ((<|>))
import           Control.Lens                          (( # ), (^.))
import qualified Data.Aeson                            as A
import           Data.Either                           (Either)
import           Data.Foldable                         (sequenceA_)
import           Data.Proxy
import           Data.String.Conversions               (cs)
import           Data.Text                             (Text)
import qualified Data.Validation                       as V
import           GHC.Generics                          (Generic)
import           GHC.TypeLits                          (symbolVal)
import           Nameservice.Aeson                     (defaultNameserviceOptions)
import           Nameservice.Modules.Nameservice.Types (Name (..),
                                                        NameserviceModule)
import           Nameservice.Modules.Token             (Amount (..))
import           Proto3.Suite                          (Message, Named)
import           Tendermint.SDK.Types.Address          (Address,
                                                        addressFromBytes)
import           Tendermint.SDK.Types.Message          (DecodingOption (..),
                                                        Msg (..),
                                                        ParseMessage (..),
                                                        ValidateMessage (..),
                                                        isAuthorCheck,
                                                        nonEmptyCheck)

data NameserviceMessage =
    NSetName SetName
  | NBuyName BuyName
  | NDeleteName DeleteName
  | NFaucetAccount FaucetAccount
  deriving (Eq, Show, Generic)

data FaucetAccount = FaucetAccount
  { faucetAccountTo     :: Address
  , faucetAccountAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Message FaucetAccount
instance Named FaucetAccount

-- @NOTE: .proto genration will use these type names as is
-- only field names stripped of prefixes during generation
data SetName = SetName
  { setNameName  :: Name
  , setNameOwner :: Address
  , setNameValue :: Text
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
  { buyNameBid   :: Amount
  , buyNameName  :: Name
  , buyNameValue :: Text
  , buyNameBuyer :: Address
  } deriving (Eq, Show, Generic)

instance Message BuyName
instance Named BuyName

instance {-# OVERLAPPING #-} ParseMessage 'Proto3Suite NameserviceMessage where
  decodeMessage p bs =
    -- NBuyName should be first
    fmap NBuyName (decodeMessage p bs) <>
    fmap NSetName (decodeMessage p bs) <>
    fmap NDeleteName (decodeMessage p bs) <>
    fmap NFaucetAccount (decodeMessage p bs)

instance ValidateMessage NameserviceMessage where
  validateMessage m@Msg{msgData} = case msgData of
    NBuyName msg    -> validateMessage m {msgData = msg}
    NSetName msg    -> validateMessage m {msgData = msg}
    NDeleteName msg -> validateMessage m {msgData = msg}

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

instance ValidateMessage DeleteName where
  validateMessage msg@Msg{..} =
    let DeleteName{deleteNameName} = msgData
        Name name = deleteNameName
    in sequenceA_
       [ nonEmptyCheck "Name" name
       , isAuthorCheck "Owner" msg deleteNameOwner
       ]

instance ValidateMessage BuyName where
  validateMessage msg@Msg{..} =
    let BuyName{buyNameName, buyNameValue} = msgData
        Name name = buyNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" buyNameValue
        , isAuthorCheck "Owner" msg buyNameBuyer
        ]
