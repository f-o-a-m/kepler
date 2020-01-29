module Tendermint.SDK.Modules.Bank.Messages where

import           Control.Lens                 (Wrapped (..), from, iso, view,
                                               (&), (.~), (^.))
import           Data.Bifunctor               (bimap)
import qualified Data.ProtoLens               as P
import           Data.String.Conversions      (cs)
import           Data.Validation              (Validation (..))
import           GHC.Generics                 (Generic)
import qualified Proto.Modules.Bank           as B
import qualified Proto.Modules.Bank_Fields    as B
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Modules.Auth  (Amount(..), CoinId(..))
import           Tendermint.SDK.Types.Address (Address, addressFromBytes,
                                               addressToBytes)
import           Tendermint.SDK.Types.Message (HasMessageType (..),
                                               ValidateMessage (..))

data Transfer = Transfer
  { transferTo     :: Address
  , transferFrom   :: Address
  , transferCoinId :: CoinId
  , transferAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Wrapped Transfer where
  type Unwrapped Transfer = B.Transfer

  _Wrapped' = iso t f
   where
    t Transfer {..} =
      P.defMessage
        & B.to .~ addressToBytes transferTo
        & B.from .~ addressToBytes transferFrom
        & B.cid .~ unCoinId transferCoinId
        & B.amount .~ unAmount transferAmount
    f message = Transfer
      { transferTo = addressFromBytes $ message ^. B.to
      , transferFrom = addressFromBytes $ message ^. B.from
      , transferCoinId = CoinId $ message ^. B.cid
      , transferAmount = Amount $ message ^. B.amount
      }

instance HasMessageType Transfer where
  messageType _ = "Transfer"

instance HasCodec Transfer where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance ValidateMessage Transfer where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------

data Burn = Burn
  { burnAddress :: Address
  , burnCoinId  :: CoinId
  , burnAmount  :: Amount
  } deriving (Eq, Show, Generic)

instance Wrapped Burn where
  type Unwrapped Burn = B.Burn

  _Wrapped' = iso t f
   where
    t Burn {..} =
      P.defMessage
        & B.address .~ addressToBytes burnAddress
        & B.cid .~ unCoinId burnCoinId
        & B.amount .~ unAmount burnAmount
    f message = Burn
      { burnAddress = addressFromBytes $ message ^. B.address
      , burnCoinId = CoinId $ message ^. B.cid
      , burnAmount = Amount $ message ^. B.amount
      }

instance HasMessageType Burn where
  messageType _ = "Burn"

instance HasCodec Burn where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance ValidateMessage Burn where
  validateMessage _ = Success ()
