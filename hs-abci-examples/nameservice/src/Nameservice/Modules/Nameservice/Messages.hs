module Nameservice.Modules.Nameservice.Messages where

import           Control.Lens                          (( # ), (^.))
import qualified Data.Aeson                            as A
import           Data.Either                           (Either)
import           Data.Foldable                         (sequenceA_)
import qualified Data.ProtoLens                        as PL
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
import qualified Proto.Nameservice.Messages            as M
import qualified Proto.Nameservice.Messages_Fields     as M
import           Tendermint.SDK.Auth                   (Address, IsMessage (..),
                                                        MessageError (..),
                                                        Msg (..),
                                                        addressFromBytes)

data NameserviceMessage =
    SetName MsgSetName
  | BuyName MsgBuyName
  | DeleteName MsgDeleteName

--instance IsMessage NameserviceMessage where
--  makeMessage m = case m of
--    SetName msg    -> (makeMessage msg) {msgData = m}
--    BuyName msg    -> (makeMessage msg) {msgData = m}
--    DeleteName msg -> (makeMessage msg) {msgData = m}

data MsgSetName =  MsgSetName
  { msgSetNameName  :: Name
  , msgSetNameValue :: String
  , msgSetNameOwner :: Address
  } deriving Generic

-- TL;DR. ValidateBasic: https://cosmos.network/docs/tutorial/set-name.html#msg
instance IsMessage MsgSetName where
  parseMessage = error "TODO: implement parseMessage SetName"
  validateMessage msg@Msg{..} =
    let MsgSetName{msgSetNameName, msgSetNameValue} = msgData
        Name name = msgSetNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" msgSetNameValue
        , isAuthorCheck "Owner" msg msgSetNameOwner
        ]

data MsgDeleteName = MsgDeleteName
  { msgDeleteNameName  :: Name
  , msgDeleteNameOwner :: Address
  } deriving Generic

instance IsMessage MsgDeleteName where
  parseMessage = error "TODO: implement parseMessage DeleteName"
  validateMessage msg@Msg{..} =
    let MsgDeleteName{msgDeleteNameName} = msgData
        Name name = msgDeleteNameName
    in sequenceA_
       [ nonEmptyCheck "Name" name
       , isAuthorCheck "Owner" msg msgDeleteNameOwner
       ]

data MsgBuyName = MsgBuyName
    { msgBuyNameName  :: Name
    , msgBuyNameValue :: String
    , msgBuyNameBuyer :: Address
    , msgBuyNameBid   :: Amount
    } deriving Generic

instance IsMessage MsgBuyName where
  parseMessage = error "TODO: implement parseMessage BuyName"
  validateMessage msg@Msg{..} =
    let MsgBuyName{msgBuyNameName, msgBuyNameValue} = msgData
        Name name = msgBuyNameName
    in sequenceA_
        [ nonEmptyCheck "Name" name
        , nonEmptyCheck "Value" msgBuyNameValue
        , isAuthorCheck "Owner" msg msgBuyNameBuyer
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
