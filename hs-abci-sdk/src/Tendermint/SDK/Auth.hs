module Tendermint.SDK.Auth
  ( AccountAddress
  , Address(..)
  , Account(..)
  , PubKey(..)
  , PrivateKey(..)
  , fromBech32
  , toBech32
  ) where

import qualified Codec.Binary.Bech32      as Bech32
import qualified Data.Aeson               as A
import qualified Data.ByteArray.HexString as Hex
import           Data.ByteString          (ByteString)
import           Data.Int                 (Int64)
import           Data.Proxy               (Proxy (..))
import           Data.String.Conversions
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           GHC.TypeLits             (KnownSymbol, symbolVal)
import           Tendermint.SDK.Aeson     (defaultSDKAesonOptions)

newtype Address = Address Hex.HexString deriving (Eq, Show, Ord, A.ToJSON, A.FromJSON)

newtype AccountAddress prefix = AccountAddress Address deriving (Eq, Show, Ord)

instance IsHumanReadable prefix => A.ToJSON (AccountAddress prefix) where
  toJSON = A.toJSON . toBech32

instance IsHumanReadable prefix => A.FromJSON (AccountAddress prefix) where
  parseJSON = A.withText "AccountAddress" $ \t ->
    either (fail . cs) pure $ fromBech32 t

fromBech32 :: forall prefix. IsHumanReadable prefix => Text -> Either Text (AccountAddress prefix)
fromBech32 a = case Bech32.decode a of
  Left e -> Left . cs . show $ e
  Right (_hrp, dp) ->
    if getPrefix (Proxy :: Proxy prefix) /= _hrp
        then Left "MismatchedHumanReadablePartError"
        else case Bech32.dataPartToBytes dp of
            Nothing -> Left "FailedToParseDataPartAsBytesError"
            Just bs -> Right  . AccountAddress . Address $ Hex.fromBytes bs

toBech32 :: IsHumanReadable prefix => AccountAddress prefix -> Text
toBech32 ((AccountAddress (Address a)) :: AccountAddress prefix) =
  Bech32.encodeLenient (getPrefix (Proxy :: Proxy prefix)) (Bech32.dataPartFromBytes . Hex.toBytes $ a)

-- | NOTE: There are rules for valid prefix p, namely
-- | 1. 1 <= length p <= 83
-- | 2. min (map chr p) >= 33
-- | 3. max (map chr p) <= 126
class KnownSymbol prefix => IsHumanReadable prefix where
  getPrefix :: Proxy prefix -> Bech32.HumanReadablePart

  default getPrefix :: Proxy prefix -> Bech32.HumanReadablePart
  getPrefix p =
    case Bech32.humanReadablePartFromText. cs $ symbolVal p of
      Left err  -> error $ show err
      Right hrp -> hrp

-- | 'pubKeyVerifyBytes' takes a message and a signature and verifies that
-- | the message was signed by this PubKey, i.e. it is a signature recovery.
data PubKey = PubKey
  { pubKeyAddress     :: Address
  , pubKeyVerifyBytes :: ByteString -> ByteString -> Bool
  }

data PrivateKey = PrivateKey
  { privateKeyPubKey :: PubKey
  , privateKeySign   :: ByteString -> Either Text ByteString
  , privateKeyRaw    :: Hex.HexString
  }

data Coin = Coin
  { coinDenomination :: Text
  , coinAmount       :: Int64
  } deriving Generic

instance A.ToJSON Coin where
  toJSON = A.genericToJSON (defaultSDKAesonOptions "coin")

data Account = Account
  { accountAddress  :: Address
  , accountPubKey   :: PubKey
  , accountCoins    :: [Coin]
  , accountNumber   :: Int64
  , accountSequence :: Int64
  }

instance A.ToJSON Account where
    toJSON Account{..} =
        A.object [ "address" A..= accountAddress
                 , "coins" A..= accountCoins
                 , "number" A..= accountNumber
                 , "sequence" A..= accountSequence
                 ]
