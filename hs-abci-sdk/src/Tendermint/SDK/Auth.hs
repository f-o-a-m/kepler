{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Auth where

import qualified Codec.Binary.Bech32      as Bech32
import           Control.Lens             (iso)
import           Control.Monad            (when)
import           Control.Monad.Catch      (Exception, MonadCatch, catch)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.Reader     (ReaderT, asks)
import qualified Data.Aeson               as A
import qualified Data.Binary              as Binary
import qualified Data.ByteArray.HexString as Hex
import           Data.ByteString          (ByteString)
import           Data.Int                 (Int64)
import           Data.IORef
import           Data.Monoid              (Endo (..))
import           Data.Proxy               (Proxy (..))
import           Data.String.Conversions
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           GHC.TypeLits             (KnownSymbol, symbolVal)
import           Polysemy
import           Tendermint.SDK.Codec     (HasCodec (..))
import           Tendermint.SDK.Store     (IsKey (..), RawKey (..))

newtype Address = Address Hex.HexString deriving (Eq, Show, Generic, Ord, A.ToJSON, A.FromJSON)

instance Binary.Binary Address where
    put (Address a) = Binary.put @ByteString . Hex.toBytes $ a
    get = Address . Hex.fromBytes <$> Binary.get @ByteString

addressToBytes :: Address -> ByteString
addressToBytes (Address addrHex) = Hex.toBytes addrHex

addressFromBytes :: ByteString -> Address
addressFromBytes = Address . Hex.fromBytes

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

data PubKey = PubKey
  { pubKeyAddress :: Address
  , pubKeyRaw     :: ByteString
  } deriving (Eq, Ord, Generic)

instance Binary.Binary PubKey

data PrivateKey = PrivateKey
  { privateKeyPubKey :: ByteString
  , privateKeyRaw    :: ByteString
  }

newtype Signature = Signature ByteString deriving Eq

data Signer = Signer
  { signerSign    :: PrivateKey -> ByteString -> Either Text Signature
  , signerRecover :: Signature -> ByteString -> PubKey
  }

signerVerify
  :: Signer
  -> PubKey
  -> Signature
  -> ByteString
  -> Bool
signerVerify Signer{signerRecover} pubKey sig msg =
    signerRecover sig msg == pubKey

data Coin = Coin
  { coinDenomination :: Text
  , coinAmount       :: Int64
  } deriving Generic

instance Binary.Binary Coin

data Account = Account
  { accountPubKey   :: PubKey
  , accountCoins    :: [Coin]
  , accountNumber   :: Int64
  , accountSequence :: Int64
  } deriving Generic

instance Binary.Binary Account

data Accounts m a where
  PutAccount :: Account -> Accounts m ()
  GetAccount :: Address -> Accounts m Account

makeSem ''Accounts

instance HasCodec Account where
    encode = cs . Binary.encode
    decode = Right . Binary.decode . cs

instance RawKey Address where
    rawKey = iso (\(Address a) -> Hex.toBytes a) (Address . Hex.fromBytes)

instance IsKey Address "auth" where
    type Value Address "auth" = Account

--------------------------------------------------------------------------------

data Msg msg = Msg
  { msgRoute      :: Text
  , msgType       :: Text
  , msgSignBytes  :: ByteString
  , msgGetSigners :: [Address]
  , msgValidate   :: Maybe Text
  , msgData       :: msg
  }

--verifyAllMsgSignatures :: Signer -> Msg msg -> Bool
--verifyAllMsgSignatures signer Msg{msgGetSigners, msgSignBytes} =
--  let isValid (pubKey, sig) = signerVerify signer pubKey sig msgSignBytes
--  in getAll . mconcat . map (All . isValid) $ msgGetSigners


data Fee = Fee
  { feeAmount :: [Coin]
  , feeGas    :: Int64
  }

data Tx tx msg = Tx
  { txMsgs       :: [Msg msg]
  , txFee        :: Fee
  , txSignatures :: [Signature]
  , txMemo       :: Text
  , txValidate   :: Maybe Text
  , txData       :: tx
  }

data GasMeter = GasMeter
  { gasMeterLimit    :: Int64
  , gasMeterConsumed :: Int64
  }

data OutOfGasException = OutOfGasException deriving (Show)

instance Exception OutOfGasException

data Context = Context
  { contextGasMeter :: GasMeter
  }

data TxEnv tx = TxEnv
  { transaction :: tx
  , context     :: IORef Context
  }

type AnteDecorator m tx msg  = ReaderT (TxEnv (Tx tx msg)) m ()

newContextDecorator
  :: forall m tx msg.
     MonadCatch m
  => MonadIO m
  => Endo (AnteDecorator m tx msg)
newContextDecorator = Endo $ \next -> do
   tx <- asks transaction
   contextVar <- asks context
   let gasMeter = GasMeter (feeGas . txFee $ tx) 0
   liftIO $ writeIORef contextVar (Context gasMeter)
   next `catch` (\(_ :: OutOfGasException) ->
     pure ()
     )

validateBasicDecorator
  :: forall m tx msg.
     Monad m
  => Endo (AnteDecorator m tx msg)
validateBasicDecorator = Endo $ \next -> do
  tx <- asks transaction
  let msgSigners = map msgGetSigners $ txMsgs tx
      expectedSignersN = length $ txSignatures tx
  when (expectedSignersN == 0) $
    error "TODO: There must be signers"
  when (any (\signers -> length signers /= expectedSignersN) msgSigners) $
    error "TODO: fill in error for wrong number of signers"
  let feeAmounts = map coinAmount . feeAmount . txFee $ tx
  when (any (< 0) feeAmounts) $
    error "TODO: No negative fees"
  next
