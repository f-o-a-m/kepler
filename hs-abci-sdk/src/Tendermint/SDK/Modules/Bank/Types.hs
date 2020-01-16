{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.Modules.Bank.Types where

import           Data.Aeson                   as A
import qualified Data.ByteArray.HexString     as Hex
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)
import           Proto3.Suite                 (HasDefault (..), MessageField,
                                               Primitive (..))
import qualified Proto3.Suite.DotProto        as DotProto
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import qualified Tendermint.SDK.BaseApp       as BaseApp
import           Tendermint.SDK.Codec         (defaultSDKAesonOptions)
import qualified Tendermint.SDK.Modules.Auth  as Auth
import           Tendermint.SDK.Types.Address (Address, addressFromBytes,
                                               addressToBytes)

--------------------------------------------------------------------------------

type BankModule = "bank"

--------------------------------------------------------------------------------

-- Address orphans
instance Primitive Address where
  encodePrimitive n a = Encode.byteString n $ addressToBytes a
  decodePrimitive = addressFromBytes <$> Decode.byteString
  primType _ = DotProto.Bytes
instance HasDefault Hex.HexString
instance HasDefault Address
instance MessageField Address

instance BaseApp.IsKey Address "bank" where
  type Value Address "bank" = Auth.Coin

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

data Faucetted = Faucetted
  { faucettedAccount :: Address
  , faucettedCoinId  :: Auth.CoinId
  , faucettedAmount  :: Auth.Amount
  } deriving (Eq, Show, Generic)

faucettedAesonOptions :: A.Options
faucettedAesonOptions = defaultSDKAesonOptions "faucetted"

instance ToJSON Faucetted where
  toJSON = A.genericToJSON faucettedAesonOptions
instance FromJSON Faucetted where
  parseJSON = A.genericParseJSON faucettedAesonOptions
instance BaseApp.ToEvent Faucetted where
  makeEventType _ = "Faucetted"
instance BaseApp.Select Faucetted

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
