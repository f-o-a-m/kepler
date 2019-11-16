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
import           Nameservice.Modules.Token             (Address (..),
                                                        Amount (..))
import           Proto3.Suite                          (Message, Named)
import           Tendermint.SDK.Auth                   (Address, IsMessage (..),
                                                        MessageError (..),
                                                        Msg (..),
                                                        addressFromBytes)

data NameserviceMessage =
    NSetName SetName
  | NBuyName BuyName
  | NDeleteName DeleteName
  deriving (Eq, Show, Generic)

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

instance IsMessage NameserviceMessage where
  fromMessage bs =
    fmap NSetName (fromMessage bs) <>
    fmap NBuyName (fromMessage bs) <>
    fmap NDeleteName (fromMessage bs)
  validateMessage m@Msg{msgData} = case msgData of
    NSetName msg    -> validateMessage m {msgData = msg}
    NBuyName msg    -> validateMessage m {msgData = msg}
    NDeleteName msg -> validateMessage m {msgData = msg}

-- TL;DR. ValidateBasic: https://cosmos.network/docs/tutorial/set-name.html#msg
instance IsMessage SetName where
  validateMessage msg@Msg{..} =
    let SetName{setNameName, setNameValue} = msgData
        Name name = setNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" setNameValue
        , isAuthorCheck "Owner" msg setNameOwner
        ]

instance IsMessage DeleteName where
  validateMessage msg@Msg{..} =
    let DeleteName{deleteNameName} = msgData
        Name name = deleteNameName
    in sequenceA_
       [ nonEmptyCheck "Name" name
       , isAuthorCheck "Owner" msg deleteNameOwner
       ]

instance IsMessage BuyName where
  validateMessage msg@Msg{..} =
    let BuyName{buyNameName, buyNameValue} = msgData
        Name name = buyNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" buyNameValue
        , isAuthorCheck "Owner" msg buyNameBuyer
        ]

--------------------------------------------------------------------------------

nonEmptyCheck
  :: Eq a
  => Monoid a
  => Text
  -> a
  -> V.Validation [MessageError] ()
nonEmptyCheck field x
  | x == mempty = V._Failure # [InvalidFieldError field "Must be nonempty."]
  | otherwise = mempty

isAuthorCheck
  :: Text
  -> Msg msg
  -> (msg -> Address)
  -> V.Validation [MessageError] ()
isAuthorCheck field Msg{msgAuthor, msgData} getAuthor
  | getAuthor msgData /= msgAuthor = V._Failure # [PermissionError field "Must be message author."]
  | otherwise = mempty
