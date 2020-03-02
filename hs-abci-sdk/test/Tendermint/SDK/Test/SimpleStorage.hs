{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Tendermint.SDK.Test.SimpleStorage
  ( SimpleStorage
  , UpdateCountTx(..)
  , UpdatePaidCountTx(..)
  , simpleStorageModule
  , simpleStorageCoinId
  , evalToIO
  , Count(..)
  ) where

import           Control.Lens                       (iso, (^.))
import           Crypto.Hash                        (SHA256 (..), hashWith)
import           Data.Bifunctor                     (first)
import           Data.ByteArray                     (convert)
import qualified Data.ByteArray.Base64String        as Base64
import           Data.ByteString                    (ByteString)
import           Data.Int                           (Int32)
import           Data.Proxy
import qualified Data.Serialize                     as Serialize
import           Data.Serialize.Text                ()
import           Data.String.Conversions            (cs)
import           Data.Validation                    (Validation (..))
import           Data.Word                          (Word64)
import           GHC.Generics                       (Generic)
import           GHC.TypeLits                       (symbolVal)
import           Polysemy
import           Polysemy.Error                     (Error, catch, throw)
import           Servant.API
import           Tendermint.SDK.Application         (Module (..), ModuleEffs)
import qualified Tendermint.SDK.BaseApp             as BA
import           Tendermint.SDK.Codec               (HasCodec (..))
import qualified Tendermint.SDK.Modules.Bank        as B
import qualified Tendermint.SDK.BaseApp.Store.Array as A
import           Tendermint.SDK.Types.Address       (Address)
import           Tendermint.SDK.Types.Message       (HasMessageType (..),
                                                     Msg (..),
                                                     ValidateMessage (..))
import           Tendermint.SDK.Types.Transaction   (Tx (..))


--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
type SimpleStorageName = "simple_storage"

data SimpleStorageNamespace

simpleStorageCoinId :: B.CoinId
simpleStorageCoinId = B.CoinId . cs . symbolVal $ Proxy @SimpleStorageName

newtype Count = Count Int32 deriving (Eq, Show, Ord, Num, Serialize.Serialize)

-- Count

data CountKey = CountKey

instance HasCodec Count where
    encode = Serialize.encode
    decode = first cs . Serialize.decode

instance BA.RawKey CountKey where
    rawKey = iso (\_ -> cs countKey) (const CountKey)
      where
        countKey :: ByteString
        countKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("count" :: String)

instance BA.IsKey CountKey SimpleStorageNamespace where
    type Value CountKey SimpleStorageNamespace = Count

instance BA.FromQueryData CountKey

instance BA.Queryable Count where
  type Name Count = "count"

newtype AmountPaid = AmountPaid B.Amount deriving (Eq, Show, Num, Ord, HasCodec)

-- for reporting how much paid to change count
instance BA.IsKey Address SimpleStorageNamespace where
    type Value Address SimpleStorageNamespace = AmountPaid

instance BA.Queryable AmountPaid where
  type Name AmountPaid = "paid"

-- | Counts

data CountsKey = CountsKey

instance BA.RawKey CountsKey where
    rawKey = iso (\_ -> cs countsKey) (const CountsKey)
      where
        countsKey :: ByteString
        countsKey = convert . hashWith SHA256 . cs @_ @ByteString $ ("counts" :: String)

instance BA.IsKey CountsKey SimpleStorageNamespace where
  type Value CountsKey SimpleStorageNamespace = A.Array Count

--------------------------------------------------------------------------------
-- Message Types
--------------------------------------------------------------------------------

data UpdateCountTx = UpdateCountTx
  { updateCountTxCount    :: Int32
  } deriving (Show, Eq, Generic)

instance Serialize.Serialize UpdateCountTx

instance HasMessageType UpdateCountTx where
  messageType _ = "update_count"

instance HasCodec UpdateCountTx where
  encode = Serialize.encode
  decode = first cs . Serialize.decode

instance ValidateMessage UpdateCountTx where
  validateMessage _ = Success ()

data UpdatePaidCountTx = UpdatePaidCountTx
  { updatePaidCountTxCount  :: Int32
  , updatePaidCountTxAmount :: Word64
  } deriving (Show, Eq, Generic)

instance Serialize.Serialize UpdatePaidCountTx

instance HasMessageType UpdatePaidCountTx where
  messageType _ = "update_paid_count"

instance HasCodec UpdatePaidCountTx where
  encode = Serialize.encode
  decode = first cs . Serialize.decode

instance ValidateMessage UpdatePaidCountTx where
  validateMessage _ = Success ()

--------------------------------------------------------------------------------
-- Keeper
--------------------------------------------------------------------------------

store :: BA.Store SimpleStorageNamespace
store = BA.makeStore $ BA.KeyRoot (cs . symbolVal $ Proxy @SimpleStorageName)

countsList :: A.Array Count
countsList = A.makeArray CountsKey store

data SimpleStorageKeeper m a where
    PutCount :: Count -> SimpleStorageKeeper m ()
    GetCount :: SimpleStorageKeeper m (Maybe Count)
    StoreAmountPaid :: Address -> AmountPaid -> SimpleStorageKeeper m ()
    GetAllCounts :: SimpleStorageKeeper m [Count]

makeSem ''SimpleStorageKeeper

updateCount
  :: Member SimpleStorageKeeper r
  => Count
  -> Sem r ()
updateCount count =
  putCount count


updatePaidCount
  :: forall r .
     Member SimpleStorageKeeper r
  => Members B.BankEffs r
  => Members BA.BaseEffs r
  => Address
  -> Count
  -> B.Amount
  -> Sem r ()
updatePaidCount from count amount =
  catch @_ @r
    ( do
        B.burn from (B.Coin simpleStorageCoinId amount)
        updateCount count
        storeAmountPaid from (AmountPaid amount)
    )
    (\(B.InsufficientFunds _) -> do
      let mintAmount = B.Coin simpleStorageCoinId (amount + 1)
      B.mint from mintAmount
      updatePaidCount from count amount
    )

type SimpleStorageEffs = '[SimpleStorageKeeper]

eval
  :: forall r.
     Members BA.TxEffs r
  => forall a. (Sem (SimpleStorageKeeper ': r) a -> Sem r a)
eval = interpret (\case
  PutCount count -> do
    BA.put store CountKey count
    A.append count countsList
  GetCount -> BA.get store CountKey
  StoreAmountPaid from amt -> BA.put store from amt
  GetAllCounts -> A.toList countsList
  )

--------------------------------------------------------------------------------
-- Router
--------------------------------------------------------------------------------

type MessageApi =
  BA.TypedMessage UpdateCountTx BA.:~> BA.Return () :<|>
  BA.TypedMessage UpdatePaidCountTx BA.:~> BA.Return ()

messageHandlers
  :: Member SimpleStorageKeeper r
  => Members B.BankEffs r
  => Members BA.BaseEffs r
  => BA.RouteTx MessageApi r
messageHandlers = updateCountH :<|> updatePaidCountH

updateCountH
  :: Member SimpleStorageKeeper r
  => BA.RoutingTx UpdateCountTx
  -> Sem r ()
updateCountH (BA.RoutingTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
      UpdateCountTx{updateCountTxCount} = msgData
  in updateCount (Count updateCountTxCount)

updatePaidCountH
  :: Member SimpleStorageKeeper r
  => Members B.BankEffs r
  => Members BA.BaseEffs r
  => BA.RoutingTx UpdatePaidCountTx
  -> Sem r ()
updatePaidCountH (BA.RoutingTx Tx{txMsg}) =
  let Msg{msgData, msgAuthor} = txMsg
      UpdatePaidCountTx{..} = msgData
  in updatePaidCount msgAuthor (Count updatePaidCountTxCount)
       (B.Amount updatePaidCountTxAmount)

--------------------------------------------------------------------------------
-- Server
--------------------------------------------------------------------------------

type CountStoreContents =
  '[ (CountKey, Count)
   , (Address, AmountPaid)
   ]

type GetMultipliedCount =
     "manipulated"
  :> Capture "subtract" Integer
  :> QueryParam' '[Required, Strict] "factor" Integer
  :> BA.Leaf Count

getMultipliedCount
  :: Members [Error BA.AppError, SimpleStorageKeeper] r
  => Integer
  -> Integer
  -> Sem r (BA.QueryResult Count)
getMultipliedCount subtractor multiplier = do
  let m = fromInteger multiplier
      s = fromInteger subtractor
  mc <- getCount
  case mc of
    Nothing -> throw . BA.makeAppError $ BA.ResourceNotFound
    Just c -> pure $ BA.QueryResult
      { queryResultData = m * c - s
      , queryResultIndex = 0
      , queryResultKey = Base64.fromBytes $ CountKey ^. BA.rawKey
      , queryResultProof  = Nothing
      , queryResultHeight = 0
      }

type QueryApi = GetMultipliedCount :<|> BA.QueryApi CountStoreContents

querier
  :: forall r.
     Members BA.QueryEffs r
  => Member SimpleStorageKeeper r
  => BA.RouteQ QueryApi r
querier =
  let storeHandlers = BA.storeQueryHandlers (Proxy :: Proxy CountStoreContents)
        store (Proxy :: Proxy r)
  in getMultipliedCount :<|> storeHandlers

--------------------------------------------------------------------------------
-- Module Definition
--------------------------------------------------------------------------------

type SimpleStorage =
  Module SimpleStorageName MessageApi MessageApi QueryApi SimpleStorageEffs '[B.Bank]

simpleStorageModule
  :: Members (ModuleEffs SimpleStorage) r
  => SimpleStorage r
simpleStorageModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = BA.defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }

evalToIO
  :: BA.PureContext
  -> Sem (BA.BaseAppEffs BA.PureCoreEffs) a
  -> IO a
evalToIO context action = do
  eRes <- BA.runPureCoreEffs context .  BA.defaultCompileToPureCore $ action
  either (error . show) pure eRes
