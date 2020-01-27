{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.Modules.Bank.Types where

import           Data.Aeson                   as A
import qualified Data.ByteArray.HexString     as Hex
import           Data.String                  (fromString)
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text, unpack)
import           GHC.Generics                 (Generic)
import           Proto3.Suite                 (HasDefault (..), MessageField,
                                               Primitive (..))
import qualified Proto3.Suite.DotProto        as DotProto
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import qualified Tendermint.SDK.BaseApp       as BaseApp
import           Tendermint.SDK.Codec         (HasCodec (..),
                                               defaultSDKAesonOptions)
import qualified Tendermint.SDK.Modules.Auth  as Auth
import           Tendermint.SDK.Types.Address (Address (..), addressFromBytes,
                                               addressToBytes)
import           Web.HttpApiData              (FromHttpApiData (..),
                                               ToHttpApiData (..))

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
instance HasCodec Address where
  decode = Right . addressFromBytes
  encode = addressToBytes
instance ToHttpApiData Address where
  toQueryParam (Address aHex) = Hex.format aHex
instance FromHttpApiData Address where
  parseQueryParam = Right . Address . fromString . unpack

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data BankError =
    InsufficientFunds Text
  | PutOnNonExistentAccount Address

instance BaseApp.IsAppError BankError where
  makeAppError (InsufficientFunds msg) =
    BaseApp.AppError
    { appErrorCode = 1
    , appErrorCodespace = "bank"
    , appErrorMessage = msg
    }
  makeAppError (PutOnNonExistentAccount addr) =
    BaseApp.AppError
    { appErrorCode = 2
    , appErrorCodespace = "bank"
    , appErrorMessage = "Attempted to put balance on non-existent account: " <> (cs . show $ addr)
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
