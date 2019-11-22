{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nameservice.Modules.Token.Types where

import           Data.Aeson                   as A
import           Data.Bifunctor               (bimap)
import qualified Data.ByteArray.HexString     as Hex
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           Nameservice.Aeson            (defaultNameserviceOptions)
import           Proto3.Suite                 (HasDefault (..), MessageField,
                                               Primitive (..))
import qualified Proto3.Suite.DotProto        as DotProto
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import           Proto3.Wire.Types            (fieldNumber)
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Errors        (AppError (..), IsAppError (..))
import           Tendermint.SDK.Events        (FromEvent, ToEvent (..))
import           Tendermint.SDK.Query         (Queryable (..))
import qualified Tendermint.SDK.Store         as Store
import           Tendermint.SDK.Types.Address (Address, addressFromBytes,
                                               addressToBytes)

--------------------------------------------------------------------------------

type TokenModule = "token"

--------------------------------------------------------------------------------

newtype Amount = Amount Word64 deriving (Eq, Show, Num, Generic, Ord, A.ToJSON, A.FromJSON)
instance Primitive Amount where
  encodePrimitive n (Amount amt) = Encode.uint64 n amt
  decodePrimitive = Amount <$> Decode.uint64
  primType _ = DotProto.UInt64
instance HasDefault Amount
instance MessageField Amount

instance Queryable Amount where
  type Name Amount = "balance"

-- @NOTE: hacks
instance HasCodec Amount where
  encode (Amount b) =
    -- proto3-wire only exports encoders for message types
    let dummyMsgEncoder = Encode.uint64 (fieldNumber 1)
    in cs . Encode.toLazyByteString . dummyMsgEncoder $ b
  decode = bimap (cs . show) Amount . Decode.parse dummyMsgParser
    where
      -- field is always present; 0 is an arbitrary value
      fieldParser = Decode.one Decode.uint64 0
      dummyMsgParser = Decode.at fieldParser (fieldNumber 1)

-- orphans
instance Primitive Address where
  encodePrimitive n a = Encode.byteString n $ addressToBytes a
  decodePrimitive = addressFromBytes <$> Decode.byteString
  primType _ = DotProto.Bytes
instance HasDefault Hex.HexString
instance HasDefault Address
instance MessageField Address

instance Store.IsKey Address "token" where
  type Value Address "token" = Amount

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data TokenException =
    InsufficientFunds Text

instance IsAppError TokenException where
    makeAppError (InsufficientFunds msg) =
      AppError
        { appErrorCode = 1
        , appErrorCodespace = "token"
        , appErrorMessage = msg
        }

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data Faucetted = Faucetted
  { faucettedAccount :: Address
  , faucettedAmount  :: Amount
  } deriving (Eq, Show, Generic)

faucettedAesonOptions :: A.Options
faucettedAesonOptions = defaultNameserviceOptions "faucetted"

instance ToJSON Faucetted where
  toJSON = A.genericToJSON faucettedAesonOptions
instance FromJSON Faucetted where
  parseJSON = A.genericParseJSON faucettedAesonOptions
instance ToEvent Faucetted where
  makeEventType _ = "Faucetted"
instance FromEvent Faucetted

data TransferEvent = TransferEvent
  { transferEventAmount :: Amount
  , transferEventTo     :: Address
  , transferEventFrom   :: Address
  } deriving Generic

transferEventAesonOptions :: A.Options
transferEventAesonOptions = defaultNameserviceOptions "transferEvent"

instance A.ToJSON TransferEvent where
  toJSON = A.genericToJSON transferEventAesonOptions

instance A.FromJSON TransferEvent where
  parseJSON = A.genericParseJSON transferEventAesonOptions

instance ToEvent TransferEvent where
  makeEventType _ = "TransferEvent"

instance FromEvent TransferEvent
