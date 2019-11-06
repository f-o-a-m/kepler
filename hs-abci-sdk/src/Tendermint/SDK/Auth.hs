module Tendermint.SDK.Auth where

import qualified Codec.Binary.Bech32      as Bech32
import           Control.Monad            (when)
import           Control.Monad.Catch      (Exception, MonadCatch, catch)
import           Control.Monad.IO.Class   (MonadIO (..))
import           Control.Monad.Reader     (ReaderT, asks)
import qualified Data.Aeson               as A
import qualified Data.ByteArray.HexString as Hex
import           Data.ByteString          (ByteString)
import           Data.Int                 (Int64)
import           Data.IORef
import           Data.Monoid              (Endo (..), All(..))
import           Data.Proxy               (Proxy (..))
import           Data.String.Conversions
import           Data.Text                (Text)
import           GHC.Generics             (Generic)
import           GHC.TypeLits             (KnownSymbol, symbolVal)
import           Tendermint.SDK.Aeson     (defaultSDKAesonOptions)

newtype Address = Address Hex.HexString deriving (Eq, Show, Ord, A.ToJSON, A.FromJSON)

addressToBytes :: Address -> ByteString
addressToBytes (Address addrHex) = Hex.toBytes addrHex

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

verifyAccount :: Account -> Bool
verifyAccount Account{..} = accountAddress == pubKeyAddress accountPubKey

--------------------------------------------------------------------------------

data Msg msg = Msg
  { msgRoute      :: Text
  , msgType       :: Text
  , msgSignBytes  :: ByteString
  , msgGetSigners :: [PubKey]
  , msgValidate   :: Maybe Text
  , msgData       :: msg
  }

verifyAllMsgSignatures :: Msg msg -> Bool
verifyAllMsgSignatures Msg{msgGetSigners, msgSignBytes} = 
  let isValid msgBytes PubKey{pubKeyAddress, pubKeyVerifyBytes} =
        pubKeyVerifyBytes msgBytes (addressToBytes pubKeyAddress)
  in getAll . mconcat . map (All . isValid msgSignBytes) $ msgGetSigners


data Fee = Fee
  { feeAmount :: [Coin]
  , feeGas    :: Int64
  }

data Signature = Signature
  { signaturePubKey :: PubKey
  , signatureBytes  :: ByteString
  }

verifySignature :: Signature -> Bool
verifySignature Signature{signaturePubKey, signatureBytes} = 
  let PubKey{pubKeyAddress, pubKeyVerifyBytes} = signaturePubKey
  in pubKeyVerifyBytes signatureBytes (addressToBytes pubKeyAddress)

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
  when (length (filter (\signers -> length signers /= expectedSignersN) msgSigners) /= 0) $
    error "TODO: fill in error for wrong number of signers"
  let feeAmounts = map coinAmount . feeAmount . txFee $ tx
  when (filter (< 0) feeAmounts /= []) $
    error "TODO: No negative fees"
  next