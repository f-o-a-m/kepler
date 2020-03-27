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
import           Tendermint.SDK.Modules.Auth  (Amount (..), CoinId (..))
import           Tendermint.SDK.Types.Address (Address, addressFromBytes,
                                               addressToBytes)
import           Tendermint.SDK.Types.Message (HasMessageType (..),
                                               ValidateMessage (..))

data TransferMsg = TransferMsg
  { transferTo     :: Address
  , transferFrom   :: Address
  , transferCoinId :: CoinId
  , transferAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Wrapped TransferMsg where
  type Unwrapped TransferMsg = B.Transfer

  _Wrapped' = iso t f
   where
    t TransferMsg {..} =
      P.defMessage
        & B.to .~ addressToBytes transferTo
        & B.from .~ addressToBytes transferFrom
        & B.cid .~ unCoinId transferCoinId
        & B.amount .~ unAmount transferAmount
    f message = TransferMsg
      { transferTo = addressFromBytes $ message ^. B.to
      , transferFrom = addressFromBytes $ message ^. B.from
      , transferCoinId = CoinId $ message ^. B.cid
      , transferAmount = Amount $ message ^. B.amount
      }

instance HasMessageType TransferMsg where
  messageType _ = "TransferMsg"

instance HasCodec TransferMsg where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance ValidateMessage TransferMsg where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------

data BurnMsg = BurnMsg
  { burnAddress :: Address
  , burnCoinId  :: CoinId
  , burnAmount  :: Amount
  } deriving (Eq, Show, Generic)

instance Wrapped BurnMsg where
  type Unwrapped BurnMsg = B.Burn

  _Wrapped' = iso t f
   where
    t BurnMsg {..} =
      P.defMessage
        & B.address .~ addressToBytes burnAddress
        & B.cid .~ unCoinId burnCoinId
        & B.amount .~ unAmount burnAmount
    f message = BurnMsg
      { burnAddress = addressFromBytes $ message ^. B.address
      , burnCoinId = CoinId $ message ^. B.cid
      , burnAmount = Amount $ message ^. B.amount
      }

instance HasMessageType BurnMsg where
  messageType _ = "BurnMsg"

instance HasCodec BurnMsg where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance ValidateMessage BurnMsg where
  validateMessage _ = Success ()
