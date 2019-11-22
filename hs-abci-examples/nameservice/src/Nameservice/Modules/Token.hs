{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Nameservice.Modules.Token
  (
  -- * types
    Address(..)
  , Amount(..)
  , TokenException(..)
  , Transfer(..)

  -- * effects
  , Token
  , TokenEffR
  , HasTokenEff
  , getBalance
  , transfer
  , mint
  , burn

  -- * interpreter
  , eval

  -- * Query Api
  , Api
  , server

  ) where

import           Data.Aeson                   as A
import           Data.Bifunctor               (bimap)
import qualified Data.ByteArray.HexString     as Hex
import           Data.Maybe                   (fromMaybe)
import           Data.Proxy
import           Data.String.Conversions      (cs)
import           Data.Text                    (Text)
import           Data.Word                    (Word64)
import           GHC.Generics                 (Generic)
import           Nameservice.Aeson            (defaultNameserviceOptions)
import           Polysemy
import           Polysemy.Error               (Error, mapError, throw)
import           Polysemy.Output              (Output)
import           Proto3.Suite                 (HasDefault (..), MessageField,
                                               Primitive (..))
import qualified Proto3.Suite.DotProto        as DotProto
import qualified Proto3.Wire.Decode           as Decode
import qualified Proto3.Wire.Encode           as Encode
import           Proto3.Wire.Types            (fieldNumber)
import           Servant.API                  ((:>))
import           Tendermint.SDK.BaseApp       (HasBaseAppEff)
import           Tendermint.SDK.Codec         (HasCodec (..))
import           Tendermint.SDK.Errors        (AppError (..), IsAppError (..))
import           Tendermint.SDK.Events        (Event, FromEvent, ToEvent (..),
                                               emit)
import           Tendermint.SDK.Query         (Queryable (..), RouteT)
import           Tendermint.SDK.Query.Store   (QueryApi, storeQueryHandlers)
import qualified Tendermint.SDK.Store         as Store
import           Tendermint.SDK.Types.Address (Address, addressFromBytes,
                                               addressToBytes)

newtype Amount = Amount Word64 deriving (Eq, Show, Num, Generic, Ord, A.ToJSON, A.FromJSON)
instance Primitive Amount where
  encodePrimitive n (Amount amt) = Encode.uint64 n amt
  decodePrimitive = Amount <$> Decode.uint64
  primType _ = DotProto.UInt64
instance HasDefault Amount
instance MessageField Amount

instance Queryable Amount where
  type Name Amount = "balance"

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

-- orphans
instance Primitive Address where
  encodePrimitive n a = Encode.byteString n $ addressToBytes a
  decodePrimitive = addressFromBytes <$> Decode.byteString
  primType _ = DotProto.Bytes
instance HasDefault Hex.HexString
instance HasDefault Address
instance MessageField Address

instance Store.IsKey Address "token" where
  type Value Address "token" = Amount

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

data Transfer = Transfer
  { transferAmount :: Amount
  , transferTo     :: Address
  , transferFrom   :: Address
  } deriving (Eq, Show, Generic)

transferAesonOptions :: A.Options
transferAesonOptions = defaultNameserviceOptions "transfer"

instance A.ToJSON Transfer where
  toJSON = A.genericToJSON transferAesonOptions

instance A.FromJSON Transfer where
  parseJSON = A.genericParseJSON transferAesonOptions

instance ToEvent Transfer where
  makeEventType _ = "Transfer"

instance FromEvent Transfer

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data TokenException =
    InsufficientFunds Text

instance IsAppError TokenException where
    makeAppError (InsufficientFunds msg) =
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
type HasTokenEff r = (Members TokenEffR r, Member (Output Event) r)

storeKey :: Store.StoreKey "token"
storeKey = Store.StoreKey "token"

eval
  :: HasBaseAppEff r
  => Sem (Token ': Error TokenException ': r) a
  -> Sem r a
eval = mapError makeAppError . evalToken
 where
  evalToken
    :: HasBaseAppEff r
    => Sem (Token ': r) a
    -> Sem r a
  evalToken =
    interpret (\case
        GetBalance' address ->
          Store.get storeKey address
        PutBalance address balance ->
          Store.put storeKey address balance
      )
--------------------------------------------------------------------------------
-- | Server
--------------------------------------------------------------------------------

type TokenContents = '[(Address, Amount)]

type Api = "token" :> QueryApi TokenContents

server :: Member Store.RawStore r => RouteT Api (Sem r)
server =
  storeQueryHandlers (Proxy :: Proxy TokenContents) storeKey (Proxy :: Proxy (Sem r))

--------------------------------------------------------------------------------

getBalance
  :: Member Token r
  => Address
  -> Sem r Amount
getBalance address =
  fromMaybe (Amount 0) <$> getBalance' address

transfer
  :: HasTokenEff r
  => Address
  -> Amount
  -> Address
  -> Sem r ()
transfer addr1 amount addr2 = do
  -- check if addr1 has amt
  addr1Bal <- getBalance addr1
  if addr1Bal > amount
    then do
      addr2Bal <- getBalance addr2
      let newBalance1 = addr1Bal - amount
          newBalance2 = addr2Bal + amount
      -- update both balances
      putBalance addr1 newBalance1
      putBalance addr2 newBalance2
      emit $ Transfer
        { transferAmount = amount
        , transferTo = addr2
        , transferFrom = addr1
        }
    else throw (InsufficientFunds "Insufficient funds for transfer.")

burn
  :: Members '[Token, Error TokenException] r
  => Address
  -> Amount
  -> Sem r ()
burn addr amount = do
  bal <- getBalance addr
  if bal < amount
    then throw $ InsufficientFunds "Insufficient funds for burn."
    else putBalance addr (bal - amount)

mint
  :: Members '[Token, Error TokenException] r
  => Address
  -> Amount
  -> Sem r ()
mint addr amount = do
  bal <- getBalance addr
  putBalance addr (bal + amount)

--------------------------------------------------------------------------------
