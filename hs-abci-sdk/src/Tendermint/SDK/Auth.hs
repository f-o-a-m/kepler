{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Auth where

import           Data.Bifunctor                (first)
import           Data.Proxy
import qualified Data.Serialize                as Serialize
import           Data.Serialize.Text           ()
import           Data.String.Conversions       (cs)
import           Data.Text                     (Text)
import           Data.Word
import           GHC.Generics                  (Generic)
import           GHC.TypeLits                  (symbolVal)
import           Polysemy
import           Polysemy.Error                (Error, mapError)
import           Tendermint.SDK.BaseApp.Errors (AppError (..), IsAppError (..))
import           Tendermint.SDK.BaseApp.Query  (Queryable (..))
import           Tendermint.SDK.BaseApp.Store  (IsKey (..), RawStore,
                                                StoreKey (..), get, put)
import           Tendermint.SDK.Codec          (HasCodec (..))
import           Tendermint.SDK.Types.Address  (Address)

--------------------------------------------------------------------------------

data AuthModule
type AuthModuleSym = "auth"

--------------------------------------------------------------------------------

data Coin = Coin
  { coinDenomination :: Text
  , coinAmount       :: Word64
  } deriving Generic

instance Serialize.Serialize Coin

data Account = Account
  { coins :: [Coin]
  , nonce :: Word64
  } deriving Generic

instance Serialize.Serialize Account

instance HasCodec Account where
    encode = Serialize.encode
    decode = first cs . Serialize.decode

instance IsKey Address AuthModule where
    type Value Address AuthModule = Account

instance Queryable Account where
  type Name Account = "account"

--------------------------------------------------------------------------------

data AuthError =
    RecoveryError Text
  | TransactionParseError Text

instance IsAppError AuthError where
  makeAppError (RecoveryError msg) = AppError
    { appErrorCode = 1
    , appErrorCodespace = cs . symbolVal $ (Proxy :: Proxy AuthModuleSym)
    , appErrorMessage = "Signature Recovery Error: " <> msg
    }
  makeAppError (TransactionParseError msg) = AppError
    { appErrorCode = 2
    , appErrorCodespace = cs . symbolVal $ (Proxy :: Proxy AuthModuleSym)
    , appErrorMessage = msg
    }

--------------------------------------------------------------------------------

data Accounts m a where
  PutAccount :: Address -> Account -> Accounts m ()
  GetAccount :: Address -> Accounts m (Maybe Account)

makeSem ''Accounts

type AuthEffs = [Accounts, Error AuthError]

storeKey :: StoreKey AuthModule
storeKey = StoreKey "auth"

eval
  :: Members [RawStore, Error AppError] r
  => Sem (Accounts ': Error AuthError ': r) a
  -> Sem r a
eval = mapError makeAppError . evalAuth
  where
    evalAuth
      :: Members [RawStore, Error AppError] r
      => Sem (Accounts ': r) a
      -> Sem r a
    evalAuth =
      interpret (\case
          GetAccount addr ->
            get storeKey addr
          PutAccount addr acnt ->
            put storeKey addr acnt
        )
