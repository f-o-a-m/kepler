{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.Modules.Auth.Types where

import           Control.Lens                 (Wrapped (..), from, iso, view,
                                               (&), (.~), (^.), (^..),
                                               _Unwrapped')
import           Data.Aeson                   as JSON
import           Data.Bifunctor               (bimap)
import qualified Data.ProtoLens               as P
import           Data.Proxy                   (Proxy (..))
import           Data.String                  (IsString (..))
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text, pack)
import qualified Data.Text.Lazy               as TL
import           Data.Word
import           GHC.Generics                 (Generic)
import           GHC.TypeLits                 (symbolVal)
import qualified Proto.Modules.Auth           as A
import qualified Proto.Modules.Auth_Fields    as A
import           Proto3.Suite                 (HasDefault (..), MessageField,
                                               Primitive (..), fieldNumber)
import qualified Proto3.Suite.DotProto        as DotProto
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import           Tendermint.SDK.BaseApp       (AppError (..), IsAppError (..),
                                               IsKey (..), Queryable (..))
import           Tendermint.SDK.Codec         (HasCodec (..),
                                               defaultSDKAesonOptions)
import           Tendermint.SDK.Types.Address (Address)
import           Web.HttpApiData              (FromHttpApiData (..),
                                               ToHttpApiData (..))

type AuthModule = "auth"

newtype CoinId = CoinId { unCoinId :: Text } deriving (Eq, Show, Generic)

instance IsString CoinId where
  fromString = CoinId . pack
instance Primitive CoinId where
  encodePrimitive n = Encode.text n . TL.fromStrict . unCoinId
  decodePrimitive = CoinId . TL.toStrict <$> Decode.text
  primType _ = DotProto.String
instance HasDefault CoinId
instance MessageField CoinId
instance JSON.ToJSON CoinId where
  toJSON = JSON.genericToJSON JSON.defaultOptions
instance JSON.FromJSON CoinId where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions
instance ToHttpApiData CoinId where
  toQueryParam = unCoinId
instance FromHttpApiData CoinId where
  parseQueryParam = fmap CoinId . parseQueryParam

newtype Amount = Amount { unAmount :: Word64 }
  deriving (Eq, Show, Num, Generic, Ord, JSON.ToJSON, JSON.FromJSON)

instance Primitive Amount where
  encodePrimitive n (Amount amt) = Encode.uint64 n amt
  decodePrimitive = Amount <$> Decode.uint64
  primType _ = DotProto.UInt64
instance HasDefault Amount
instance MessageField Amount

-- instance Queryable Amount where
--   type Name Amount = "balance"

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

data Coin = Coin
  { coinId     :: CoinId
  , coinAmount :: Amount
  } deriving (Eq, Show, Generic)

instance Wrapped Coin where
  type Unwrapped Coin = A.Coin

  _Wrapped' = iso t f
   where
    t Coin {..} =
      P.defMessage
        & A.id .~ unCoinId coinId
        & A.amount .~ unAmount coinAmount
    f message = Coin
      { coinId = CoinId $ message ^. A.id
      , coinAmount = Amount $ message ^. A.amount
      }

instance HasCodec Coin where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

coinAesonOptions :: JSON.Options
coinAesonOptions = defaultSDKAesonOptions "coin"

instance Queryable Coin where
  type Name Coin = "balance"


-- instance JSON.ToJSON Coin where
--   toJSON = JSON.genericToJSON coinAesonOptions
-- instance JSON.FromJSON Coin where
--   parseJSON = JSON.genericParseJSON coinAesonOptions
-- instance Primitive Coin where
--   encodePrimitive n = Encode.byteString n . Codec.encode
--   decodePrimitive =
--     let parser :: Decode.Parser Decode.RawPrimitive (Either Text Coin)
--         parser = Codec.decode <$> Decode.byteString
--     in either (error "@TODO: define left for coin primitive") id <$> parser
--   primType _ = DotProto.Bytes
-- instance HasDefault Coin
-- instance MessageField Coin

data Account = Account
  { accountCoins :: [Coin]
  , accountNonce :: Word64
  } deriving Generic

instance Wrapped Account where
  type Unwrapped Account = A.Account

  _Wrapped' = iso t f
   where
    t Account {..} =
      P.defMessage
        & A.coins .~ accountCoins ^.. traverse . _Wrapped'
        & A.nonce .~ accountNonce
    f message = Account
      { accountCoins = message ^.. A.coins. traverse . _Unwrapped'
      , accountNonce = message ^. A.nonce
      }

instance HasCodec Account where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance IsKey Address AuthModule where
    type Value Address AuthModule = Account

instance Queryable Account where
  type Name Account = "account"

data AuthError =
  AccountAlreadyExists Address

instance IsAppError AuthError where
  makeAppError (AccountAlreadyExists addr) =
    AppError
      { appErrorCode = 1
      , appErrorCodespace = cs (symbolVal $ Proxy @AuthModule)
      , appErrorMessage = "Account Already Exists " <> (cs . show $ addr) <> "."
      }
