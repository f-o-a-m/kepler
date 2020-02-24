module Tendermint.SDK.Modules.Auth.Types
  ( module Tendermint.SDK.Modules.Auth.Types
  , Address(..)
  ) where

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
import           Data.Word
import           GHC.Generics                 (Generic)
import           GHC.TypeLits                 (symbolVal)
import qualified Proto.Modules.Auth           as A
import qualified Proto.Modules.Auth_Fields    as A
import           Tendermint.SDK.BaseApp       (AppError (..), IsAppError (..),
                                               IsKey (..), Queryable (..))
import           Tendermint.SDK.Codec         (HasCodec (..),
                                               defaultSDKAesonOptions)
import           Tendermint.SDK.Types.Address (Address (..))
import           Web.HttpApiData              (FromHttpApiData (..),
                                               ToHttpApiData (..))

--------------------------------------------------------------------------------

type AuthName = "auth"

data AuthNamespace

instance IsKey Address AuthNamespace where
  type Value Address AuthNamespace = Account

instance Queryable Account where
  type Name Account = "account"

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data AuthError =
  AccountAlreadyExists Address

instance IsAppError AuthError where
  makeAppError (AccountAlreadyExists addr) =
    AppError
      { appErrorCode = 1
      , appErrorCodespace = cs (symbolVal $ Proxy @AuthName)
      , appErrorMessage = "Account Already Exists " <> (cs . show $ addr) <> "."
      }

--------------------------------------------------------------------------------

newtype CoinId = CoinId { unCoinId :: Text } deriving (Eq, Show, Generic)

instance Wrapped CoinId where
  type Unwrapped CoinId = A.CoinId

  _Wrapped' = iso t f
   where
    t CoinId {..} =
      P.defMessage
        & A.id .~ unCoinId
    f message = CoinId
      { unCoinId = message ^. A.id
      }

instance HasCodec CoinId where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

instance IsString CoinId where
  fromString = CoinId . pack
instance JSON.ToJSON CoinId where
  toJSON = JSON.genericToJSON JSON.defaultOptions
instance JSON.FromJSON CoinId where
  parseJSON = JSON.genericParseJSON JSON.defaultOptions
instance ToHttpApiData CoinId where
  toQueryParam = unCoinId
instance FromHttpApiData CoinId where
  parseQueryParam = fmap CoinId . parseQueryParam

--------------------------------------------------------------------------------

newtype Amount = Amount { unAmount :: Word64 }
  deriving (Eq, Show, Num, Generic, Ord, JSON.ToJSON, JSON.FromJSON)

instance Wrapped Amount where
  type Unwrapped Amount = A.Amount

  _Wrapped' = iso t f
   where
    t Amount {..} =
      P.defMessage
        & A.amount .~ unAmount
    f message = Amount
      { unAmount = message ^. A.amount
      }

instance HasCodec Amount where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

--------------------------------------------------------------------------------

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
        & A.id .~ coinId ^. _Wrapped'
        & A.amount .~ coinAmount ^. _Wrapped'
    f message = Coin
      { coinId = message ^. A.id . _Unwrapped'
      , coinAmount = message ^. A.amount . _Unwrapped'
      }

instance HasCodec Coin where
  encode = P.encodeMessage . view _Wrapped'
  decode = bimap cs (view $ from _Wrapped') . P.decodeMessage

coinAesonOptions :: JSON.Options
coinAesonOptions = defaultSDKAesonOptions "coin"

instance Queryable Coin where
  type Name Coin = "balance"

--------------------------------------------------------------------------------

data Account = Account
  { accountCoins :: [Coin]
  , accountNonce :: Word64
  } deriving (Show, Generic)

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
