module Tendermint.SDK.Modules.Bank.Types where

import           Data.Aeson                   as A
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import qualified Tendermint.SDK.BaseApp       as BaseApp
import           Tendermint.SDK.Codec         (defaultSDKAesonOptions)
import qualified Tendermint.SDK.Modules.Auth  as Auth
import           Tendermint.SDK.Types.Address (Address (..))

--------------------------------------------------------------------------------

type BankModule = "bank"

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data BankError =
    InsufficientFunds Text

instance BaseApp.IsAppError BankError where
  makeAppError (InsufficientFunds msg) =
    BaseApp.AppError
    { appErrorCode = 1
    , appErrorCodespace = "bank"
    , appErrorMessage = msg
    }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data TransferEvent = TransferEvent
  { transferEventCoinId :: Auth.CoinId
  , transferEventAmount :: Auth.Amount
  , transferEventTo     :: Address
  , transferEventFrom   :: Address
  } deriving (Eq, Show, Generic)

transferEventAesonOptions :: A.Options
transferEventAesonOptions = defaultSDKAesonOptions "transferEvent"

instance A.ToJSON TransferEvent where
  toJSON = A.genericToJSON transferEventAesonOptions
instance A.FromJSON TransferEvent where
  parseJSON = A.genericParseJSON transferEventAesonOptions
instance BaseApp.ToEvent TransferEvent where
  makeEventType _ = "TransferEvent"
instance BaseApp.Select TransferEvent
