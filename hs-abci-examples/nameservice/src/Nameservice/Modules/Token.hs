{-# LANGUAGE  TemplateHaskell #-}

module Nameservice.Modules.Token 
  (
  -- * types
    Address
  , Balance

  -- * effects
  , Token
  , getBalance

  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Polysemy
import qualified Data.Binary as Binary
import qualified Tendermint.SDK.Store as Store
import Tendermint.SDK.Code (HasCodec(..))
import Data.String.Conversions (cs)
import Data.Maybe (fromJust)

tokenPrefix :: ByteString
tokenPrefix = "01"

-- NOTE : comes from auth module eventually
newtype Address = Address String deriving (Eq, Show)

newtype Balance = Balance Int32 deriving (Eq, Show)

instance HasCodec Balance where
    encode (Balance b) = cs $ Binary.encode b
    decode = Right . Balance . Binary.decode . cs 

instance Store.HasKey Balance where
    type Key Balance = Address
    rawKey = iso (\(Address a) -> tokenPrefix <> cs a)
      (Address . cs . fromJust . BS.stripPrefix tokenPrefix)

--------------------------------------------------------------------------------

data Token m a where
    PutBalance :: Address -> Balance -> Token m ()
    GetBalance' :: Address -> Token m (Maybe Balance)

makeSem ''Token

getBalance
  :: Member Token r
  => Address
  -> Sem r Balance
getBalance address =
  fromMaybe (Balance 0) <$> getBalance' address

transfer
  :: Member Token r
  => Address
  -> Balance
  -> Address
  -> Sem r ()
