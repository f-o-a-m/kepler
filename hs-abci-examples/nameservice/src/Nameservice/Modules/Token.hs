{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Token
  (
  -- * types
    Address
  , Amount
  , TokenException(..)

  -- * effects
  , Token
  , TokenEffR
  , HasTokenEff
  , getBalance
  , transfer

  -- * interpreter
  , eval

  -- * Query Api
  , Api
  , server

  ) where

import           Control.Lens                (iso)
import           Data.Bifunctor              (bimap)
import qualified Data.Binary                 as Binary
import           Data.ByteString             (ByteString)
import qualified Data.ByteString             as BS
import           Data.Int                    (Int32)
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Proxy
import           Data.String.Conversions     (cs)
import           Data.Text                   (Text)
import           GHC.Generics                (Generic)
import           Polysemy
import           Polysemy.Error              (Error, mapError, throw)
import           Polysemy.Output             (Output)
import           Servant.API                 ((:>))
import           Tendermint.SDK.BaseApp      (HasBaseApp)
import           Tendermint.SDK.Codec        (HasCodec (..))
import           Tendermint.SDK.Errors       (AppError (..), IsAppError (..))
import           Tendermint.SDK.Events       (Event, IsEvent (..), emit)
import           Tendermint.SDK.Router       (Queryable (..), RouteT)
import qualified Tendermint.SDK.Store        as Store
import           Tendermint.SDK.StoreQueries (QueryApi, storeQueryHandlers)

tokenKey :: ByteString
tokenKey = "01"

-- NOTE : comes from auth module eventually
newtype Address = Address String deriving (Eq, Show, Binary.Binary, Generic)

newtype Amount = Amount Int32 deriving (Eq, Show, Binary.Binary, Num, Generic, Ord)

instance Queryable Amount where
  type Name Amount = "balance"

instance HasCodec Amount where
    encode (Amount b) = cs $ Binary.encode b
    decode = Right . Amount . Binary.decode . cs

instance Store.HasKey Amount where
    type Key Amount = Address
    rawKey = iso (\(Address a) -> tokenKey <> cs a)
      (Address . cs . fromJust . BS.stripPrefix tokenKey)

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data TransferSuccessful = TransferSuccessful
  { transferSuccessfulAmount :: Amount
  , transferSuccessfulTo     :: Address
  , transferSuccessfulFrom   :: Address
  }

instance IsEvent TransferSuccessful where
  makeEventType _ = "TransferSuccessful"
  makeEventData TransferSuccessful{..} = bimap cs cs <$>
    [ (Binary.encode @String "amount", Binary.encode transferSuccessfulAmount)
    , (Binary.encode @String "to", Binary.encode transferSuccessfulTo)
    , (Binary.encode @String "from", Binary.encode transferSuccessfulFrom)
    ]

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data TokenException =
    InvalidTransfer Text

instance IsAppError TokenException where
    makeAppError (InvalidTransfer msg) =
      AppError
        { appErrorCode = 1
        , appErrorCodespace = "token"
        , appErrorMessage = msg
        }

--------------------------------------------------------------------------------

data Token m a where
    PutBalance :: Address -> Amount -> Token m ()
    GetBalance' :: Address -> Token m (Maybe Amount)

makeSem ''Token

type TokenEffR = '[Token, Error TokenException]
type HasTokenEff r = Members TokenEffR r

eval
  :: HasBaseApp r
  => Member (Error AppError) r
  => Sem (Token ': Error TokenException ': r) a
  -> Sem r a
eval = mapError makeAppError . evalToken
 where
  evalToken
    :: HasBaseApp r
    => Sem (Token ': r) a
    -> Sem r a
  evalToken =
    interpret (\case
        GetBalance' address ->
          Store.get (undefined :: Store.Root) address
        PutBalance address balance ->
          Store.put address balance
      )
--------------------------------------------------------------------------------
-- | Server
--------------------------------------------------------------------------------

type TokenContents = '[Amount]

type Api = "token" :> QueryApi TokenContents

server :: Member Store.RawStore r => RouteT Api (Sem r)
server = storeQueryHandlers (Proxy :: Proxy TokenContents) (Proxy :: Proxy (Sem r))

--------------------------------------------------------------------------------

getBalance
  :: Member Token r
  => Address
  -> Sem r Amount
getBalance address =
  fromMaybe (Amount 0) <$> getBalance' address

transfer
  :: Members '[Token, Output Event, Error TokenException] r
  => Address
  -> Amount
  -> Address
  -> Sem r ()
transfer addr1 amount@(Amount amt) addr2 = do
  -- check if addr1 has amt
  (Amount addr1Bal) <- getBalance addr1
  if addr1Bal > amt
    then do
      (Amount addr2Bal) <- getBalance addr2
      let newBalance1 = Amount $ addr1Bal - amt
          newBalance2 = Amount $ addr2Bal + amt
      -- update both balances
      putBalance addr1 newBalance1
      putBalance addr2 newBalance2
      emit $ TransferSuccessful
        { transferSuccessfulAmount = amount
        , transferSuccessfulTo = addr2
        , transferSuccessfulFrom = addr1
        }
    else throw (InvalidTransfer "Insufficient funds")
