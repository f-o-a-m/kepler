{-# OPTIONS_GHC -fno-warn-orphans #-}

module Tendermint.SDK.Modules.Auth.Types where

import           Control.Lens                 (Wrapped (..), from, iso, view,
                                               (&), (.~), (^.), (^..),
                                               _Unwrapped')
import           Data.Bifunctor               (bimap)
import qualified Data.ProtoLens               as P
import           Data.Proxy                   (Proxy (..))
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           Data.Word
import           GHC.Generics                 (Generic)
import           GHC.TypeLits                 (symbolVal)
import qualified Proto.Modules.Auth           as A
import qualified Proto.Modules.Auth_Fields    as A
import           Tendermint.SDK.BaseApp       (AppError (..), IsAppError (..),
                                               IsKey (..), Queryable (..))
import           Tendermint.SDK.Codec         (HasCodec (..), defaultSDKAesonOptions)
import qualified Tendermint.SDK.Codec         as Codec
import           Tendermint.SDK.Types.Address (Address)
import           Data.Aeson                   as JSON
import           Proto3.Suite                 (HasDefault (..), MessageField,
                                               Primitive (..))
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import qualified Proto3.Suite.DotProto        as DotProto

type AuthModule = "auth"

data Coin = Coin
  { coinDenomination :: Text
  , coinAmount       :: Word64
  } deriving (Eq, Show, Generic)

instance Wrapped Coin where
  type Unwrapped Coin = A.Coin

  _Wrapped' = iso t f
   where
    t Coin {..} =
      P.defMessage
        & A.denomination .~ coinDenomination
        & A.amount .~ coinAmount
    f message = Coin
      { coinDenomination = message ^. A.denomination
      , coinAmount = message ^. A.amount
      }

instance HasCodec Coin where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage


coinAesonOptions :: JSON.Options
coinAesonOptions = defaultSDKAesonOptions "coin"

instance JSON.ToJSON Coin where
  toJSON = JSON.genericToJSON coinAesonOptions
instance JSON.FromJSON Coin where
  parseJSON = JSON.genericParseJSON coinAesonOptions
instance Primitive Coin where
  encodePrimitive n = Encode.byteString n . Codec.encode
  decodePrimitive =
    let parser :: Decode.Parser Decode.RawPrimitive (Either Text Coin)
        parser = Codec.decode <$> Decode.byteString
    in either (error "@TODO: define left for coin primitive") id <$> parser
  primType _ = DotProto.Bytes
instance HasDefault Coin
instance MessageField Coin


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
