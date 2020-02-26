module Tendermint.SDK.Modules.Bank.Messages where

import           Control.Lens                 (Wrapped (..), from, iso, view,
                                               (&), (.~), (^.), _Unwrapped')
import           Data.Bifunctor               (bimap)
import qualified Data.ProtoLens               as P
import           Data.String.Conversions      (cs)
import           Data.Validation              (Validation (..))
import           GHC.Generics                 (Generic)
import qualified Proto.Modules.Bank           as B
import qualified Proto.Modules.Bank_Fields    as B
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Modules.Auth  (Amount (..), CoinId (..))
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
        & B.cid .~ transferCoinId ^. _Wrapped'
        & B.amount .~ transferAmount ^. _Wrapped'
    f message = Transfer
      { transferTo = addressFromBytes $ message ^. B.to
      , transferFrom = addressFromBytes $ message ^. B.from
      , transferCoinId = message ^. B.cid . _Unwrapped'
      , transferAmount = message ^. B.amount . _Unwrapped'
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
        & B.cid .~ burnCoinId ^. _Wrapped'
        & B.amount .~ burnAmount ^. _Wrapped'
    f message = Burn
      { burnAddress = addressFromBytes $ message ^. B.address
      , burnCoinId = message ^. B.cid . _Unwrapped'
      , burnAmount = message ^. B.amount . _Unwrapped'
      }

instance HasMessageType Burn where
  messageType _ = "Burn"

instance HasCodec Burn where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance ValidateMessage Burn where
  validateMessage _ = Success ()
