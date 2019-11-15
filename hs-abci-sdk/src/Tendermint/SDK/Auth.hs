{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Auth where

import           Control.Error                          (note)
import           Control.Lens                           (iso)
import           Crypto.Hash                            (hashWith)
import           Crypto.Hash.Algorithms                 (Keccak_256 (..))
import qualified Crypto.Secp256k1                       as Crypto
import qualified Data.Aeson                             as A
import           Data.ByteArray.Base64String            as Base64
import           Data.ByteArray.HexString               as Hex
import           Data.ByteString                        (ByteString)
import           Data.Maybe                             (fromJust)
import           Data.Proxy
import qualified Data.Serialize                         as Serialize
import           Data.Serialize.Text                    ()
import           Data.String.Conversions                (cs)
import           Data.Text                              (Text)
import           GHC.Generics                           (Generic)
import           GHC.TypeLits                           (symbolVal)
import qualified Network.ABCI.Types.Messages.FieldTypes as FT
import           Polysemy
import           Polysemy.Error                         (Error, mapError, throw)
import           Proto3.Wire.Decode                     as Wire
import           Tendermint.SDK.BaseApp                 (HasBaseApp)
import           Tendermint.SDK.Codec                   (HasCodec (..))
import           Tendermint.SDK.Errors                  (AppError (..),
                                                         IsAppError (..))
import           Tendermint.SDK.Store                   (IsKey (..),
                                                         RawKey (..),
                                                         StoreKey (..), get,
                                                         put)


import           Data.ByteArray                         (convert)
import qualified Data.Validation                        as V

--------------------------------------------------------------------------------

type AuthModule = "auth"

--------------------------------------------------------------------------------

newtype Address = Address Hex.HexString deriving (Eq, Show, Ord, A.ToJSON, A.FromJSON)

instance Serialize.Serialize Address where
    put (Address a) = Serialize.put @ByteString . Hex.toBytes $ a
    get = Address . Hex.fromBytes <$> Serialize.get @ByteString

addressToBytes :: Address -> ByteString
addressToBytes (Address addrHex) = Hex.toBytes addrHex

addressFromBytes :: ByteString -> Address
addressFromBytes = Address . Hex.fromBytes

pubKeyToAddress :: Crypto.PubKey -> Address
pubKeyToAddress = addressFromBytes . Crypto.exportPubKey False

parsePubKey :: FT.PubKey -> Either Text Crypto.PubKey
parsePubKey FT.PubKey{..}
  | pubKeyType == "secp256k1" =
      note "Couldn't parse PubKey" $ Crypto.importPubKey . Base64.toBytes $ pubKeyData
  | otherwise = Left $ "Unsupported curve: " <> pubKeyType

data Account = Account
  { accountPubKey   :: Crypto.PubKey
  } deriving Generic

instance Serialize.Serialize Account where
  put Account{..} = Serialize.put $ Crypto.exportPubKey False accountPubKey
  get = do
    pubKeyBs <- Serialize.get
    pubKey <- maybe (fail "Couldn't deserialize PubKey") return $ Crypto.importPubKey pubKeyBs
    return Account
      { accountPubKey = pubKey
      }

instance HasCodec Account where
    encode = Serialize.encode
    decode = Serialize.decode

instance RawKey Address where
    rawKey = iso (\(Address a) -> Hex.toBytes a) (Address . Hex.fromBytes)

instance IsKey Address AuthModule where
    type Value Address AuthModule = Account

--------------------------------------------------------------------------------

data AuthError =
  RecoveryError Text

instance IsAppError AuthError where
  makeAppError (RecoveryError msg) = AppError
    { appErrorCode = 1
    , appErrorCodespace = cs . symbolVal $ (Proxy :: Proxy AuthModule)
    , appErrorMessage = "Signature Recovery Error: " <> msg
    }

--------------------------------------------------------------------------------

data Accounts m a where
  PutAccount :: Address -> Account -> Accounts m ()
  GetAccount :: Address -> Accounts m (Maybe Account)

makeSem ''Accounts

type AuthEffR = [Accounts, Error AuthError]
type HasAuthEff r = Members AuthEffR r

storeKey :: StoreKey AuthModule
storeKey = StoreKey "auth"

eval
  :: HasBaseApp r
  => Member (Error AppError) r
  => Sem (Accounts ': Error AuthError ': r) a
  -> Sem r a
eval = mapError makeAppError . evalAuth
  where
    evalAuth
      :: HasBaseApp r
      => Sem (Accounts ': r) a
      -> Sem r a
    evalAuth =
      interpret (\case
          GetAccount addr ->
            get storeKey addr
          PutAccount addr acnt ->
            put storeKey addr acnt
        )

--------------------------------------------------------------------------------

recoverSignature
  :: Member (Error AuthError) r
  => Crypto.RecSig
  -> Crypto.Msg
  -> Sem r Crypto.PubKey
recoverSignature sig msg =
  case Crypto.recover sig msg of
    Nothing ->
       let hint = cs . show $ (sig, msg)
       in throw $ RecoveryError hint
    Just pk -> return pk

verifySignature
  :: Member (Error AuthError) r
  => Crypto.PubKey
  -> Crypto.RecSig
  -> Crypto.Msg
  -> Sem r Bool
verifySignature expectedKey sig msg = do
  foundKey <- recoverSignature sig msg
  return $ foundKey == expectedKey

--------------------------------------------------------------------------------

data Msg msg = Msg
  { msgAuthor :: Address
  , msgData   :: msg
  }

data MessageError =
    PermissionError Text Text
  | InvalidFieldError Text Text

-- note invalid messages should fail after signature verification,
-- because it could depend on a permission failure for example
class IsMessage msg where
  fromMessage :: Wire.Parser Wire.RawMessage msg
  validateMessage :: Msg msg -> V.Validation [MessageError] ()

data Tx msg = Tx
  { txMsg       :: Msg msg
  , txSignature :: Crypto.RecSig
  , txSignBytes :: Crypto.Msg
  , txSigner    :: Crypto.PubKey
  }

--------------------------------------------------------------------------------

keccak256 :: ByteString -> ByteString
keccak256 = convert . hashWith Keccak_256

data Transaction = Transaction
  { transactionData      :: ByteString
  , transactionSignature :: ByteString
  , transactionRoute     :: ByteString
  }

formatWireParseError :: Wire.ParseError -> Text
formatWireParseError = cs . go
  where 
    go err = 
      let (context,msg) = case err of
             Wire.WireTypeError txt -> ("Wire Type Error", txt)
             Wire.BinaryError txt -> ("Binary Error", txt)
             Wire.EmbeddedError txt err' -> ("Embedded Error", txt <> ". " <>  maybe "" go err')
      in "Parse Error [" <> context <> "]: " <> msg

parseTx
  :: forall msg r.
     Member (Error AuthError) r
  => IsMessage msg
  => Transaction
  -> Sem r (Either Wire.ParseError (Tx msg))
parseTx Transaction{..} = do
  let signBytes = fromJust . Crypto.msg . keccak256 $ transactionData
  compactRecSig <- case Serialize.decode transactionSignature of
    Right s -> return s
    Left err -> throw . RecoveryError $ "Invalid Compact Recovery Signature: " <> cs err
  recSig <- case Crypto.importCompactRecSig compactRecSig of
    Just s -> return s
    Nothing -> throw . RecoveryError $ "Invalid Recovery Signature: " <> cs (show compactRecSig)
  pubKey <- recoverSignature recSig signBytes
  return $ case Wire.parse (fromMessage @msg) transactionData of
    Left err -> Left err
    Right (msg :: msg) -> Right Tx
      { txMsg = Msg
        { msgAuthor = pubKeyToAddress pubKey
        , msgData = msg
        }
      , txSignature = recSig
      , txSignBytes = signBytes
      , txSigner = pubKey
      }
