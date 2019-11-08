{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Token
  (
  -- * types
    Address
  , Amount
  , mkAmount
  -- * effects
  , Token
  , Exception
  , getBalance
  , transfer
  ) where

import           Control.Lens            (iso)
import           Data.Bifunctor          (bimap)
import qualified Data.Binary             as Binary
import           Data.ByteString         (ByteString)
import qualified Data.ByteString         as BS
import           Data.Int                (Int32)
import           Data.Maybe              (fromJust, fromMaybe)
import           Data.String.Conversions (cs)
import           GHC.Generics            (Generic)
import           Polysemy
import           Polysemy.Error          (Error, throw)
import           Polysemy.Output         (Output)
import           Tendermint.SDK.Codec    (HasCodec (..))
import           Tendermint.SDK.Events   (Event, IsEvent (..), emit)
import qualified Tendermint.SDK.Store    as Store

tokenKey :: ByteString
tokenKey = "01"

-- NOTE : comes from auth module eventually
newtype Address = Address String deriving (Eq, Show, Binary.Binary, Generic)

-- newtype Balance = Balance Int32 deriving (Eq, Show, Binary.Binary, Generic)

newtype Amount = Amount Int32 deriving (Eq, Show, Binary.Binary, Generic)

mkAmount :: Int32 -> Amount
mkAmount = Amount

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
  , transferSuccessfulTo :: Address
  , transferSuccessfulFrom :: Address
  }

instance IsEvent TransferSuccessful where
  makeEventType _ = "TransferSuccessful"
  makeEventData TransferSuccessful{..} = (bimap cs cs) <$>
    [ (Binary.encode @String "amount", Binary.encode transferSuccessfulAmount)
    , (Binary.encode @String "to", Binary.encode transferSuccessfulTo)
    , (Binary.encode @String "from", Binary.encode transferSuccessfulFrom)
    ]

--------------------------------------------------------------------------------
-- Exceptions
--------------------------------------------------------------------------------

data Exception =
  InvalidTransfer String

--------------------------------------------------------------------------------

data Token m a where
    PutBalance :: Address -> Amount -> Token m ()
    GetBalance' :: Address -> Token m (Maybe Amount)

makeSem ''Token

getBalance
  :: Member Token r
  => Address
  -> Sem r Amount
getBalance address =
  fromMaybe (Amount 0) <$> getBalance' address

transfer
  :: Member Token r
  => Member (Output Event) r
  => Member (Error Exception) r
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
