{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Auth where

import           Control.Error                            (note)
--import qualified Codec.Binary.Bech32                      as Bech32
import           Control.Lens                             (iso)
--import           Control.Monad            (when)
--import           Control.Monad.Catch      (Exception, MonadCatch, catch)
--import           Control.Monad.IO.Class   (MonadIO (..))
--import           Control.Monad.Reader     (ReaderT, asks)
import qualified Crypto.Secp256k1                         as Crypto
import qualified Data.Aeson                               as A
import           Data.ByteArray.Base64String              as Base64
import           "hs-abci-types" Data.ByteArray.HexString as Hex
import           Data.ByteString                          (ByteString)
import           Data.Int                                 (Int64)
import           Data.Maybe                               (fromJust)
import qualified Data.Serialize                           as Serialize
import           Data.Serialize.Text                      ()
import           Data.Text                                (Text)
import           GHC.Generics                             (Generic)
import qualified Network.ABCI.Types.Messages.FieldTypes   as FT
import           Polysemy
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Tendermint.SDK.Store                     (IsKey (..),
                                                           RawKey (..))

newtype Address = Address Hex.HexString deriving (Eq, Show, Ord, A.ToJSON, A.FromJSON)

instance Serialize.Serialize Address where
    put (Address a) = Serialize.put @ByteString . Hex.toBytes $ a
    get = Address . Hex.fromBytes <$> Serialize.get @ByteString

addressToBytes :: Address -> ByteString
addressToBytes (Address addrHex) = Hex.toBytes addrHex

addressFromBytes :: ByteString -> Address
addressFromBytes = Address . Hex.fromBytes

--------------------------------------------------------------------------------

data PubKey = PubKey
  { pubKeyAddress :: Address
  , pubKeyRaw     :: Crypto.PubKey
  } deriving (Eq, Generic)

parsePubKey :: FT.PubKey -> Either Text PubKey
parsePubKey FT.PubKey{..}
  | pubKeyType == "secp256k1" = do
      pubKey <- note "Couldn't parse PubKey" $ Crypto.importPubKey . Base64.toBytes $ pubKeyData
      return PubKey
        { pubKeyAddress = Address . Hex.fromBytes . Crypto.exportPubKey False $ pubKey
        , pubKeyRaw = pubKey
        }
  | otherwise = Left $ "Unsupported curve: " <> pubKeyType

instance Serialize.Serialize PubKey where
  put PubKey{..} = Serialize.put (pubKeyAddress, Crypto.exportPubKey False pubKeyRaw)
  get = (\(a,r) ->
          let pk = fromJust . Crypto.importPubKey $ r
          in PubKey a pk
        ) <$> Serialize.get

data PrivateKey = PrivateKey
  { privateKeyPubKey :: ByteString
  , privateKeyRaw    :: ByteString
  }

data Coin = Coin
  { coinDenomination :: Text
  , coinAmount       :: Int64
  } deriving Generic

instance Serialize.Serialize Coin

data Account = Account
  { accountPubKey   :: PubKey
  , accountCoins    :: [Coin]
  , accountNumber   :: Int64
  , accountSequence :: Int64
  } deriving Generic

instance Serialize.Serialize Account

data Accounts m a where
  PutAccount :: Account -> Accounts m ()
  GetAccountByAddress :: Address -> Accounts m (Maybe Account)
  GetAccountByPubKey :: PubKey -> Accounts m (Maybe Account)

makeSem ''Accounts

instance HasCodec Account where
    encode = Serialize.encode
    decode = Serialize.decode

instance RawKey Address where
    rawKey = iso (\(Address a) -> Hex.toBytes a) (Address . Hex.fromBytes)

instance IsKey Address "auth" where
    type Value Address "auth" = Account

--------------------------------------------------------------------------------

data Msg msg = Msg
  { msgRoute     :: Text
  , msgType      :: Text
  , msgSignBytes :: ByteString
  , msgGetSigner :: Address
  , msgValidate  :: Maybe Text
  , msgData      :: msg
  }

class MakeMessage msg where
  makeMessage :: msg -> Msg msg

data Fee = Fee
  { feeAmount :: [Coin]
  , feeGas    :: Int64
  }

data Tx tx msg = Tx
  { txMsg       :: Msg msg
  , txFee       :: Fee
  , txSignature :: Crypto.RecSig
  , txMemo      :: Text
  , txValidate  :: Maybe Text
  , txData      :: tx
  }

class MakeTx tx msg where
  makeTx :: tx -> Tx tx msg
