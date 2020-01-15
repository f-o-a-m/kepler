{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Bank.Keeper where

import           Data.Maybe                           (fromMaybe)
import           Polysemy
import           Polysemy.Error                       (Error, mapError, throw)
import           Polysemy.Output                      (Output)
import qualified Tendermint.SDK.BaseApp               as BaseApp
import qualified Tendermint.SDK.Modules.Auth          as Auth
import           Tendermint.SDK.Modules.Bank.Messages (FaucetAccount (..))
import           Tendermint.SDK.Modules.Bank.Types    (BankError (..),
                                                       Faucetted (..),
                                                       TransferEvent (..))
import           Tendermint.SDK.Types.Address         (Address)
import Data.Text (Text)
import Data.List (find)

data Bank m a where
    PutBalance :: Address -> [Auth.Coin] -> Bank m ()
    GetBalance' :: Address -> Text -> Bank m (Maybe Auth.Coin)

makeSem ''Bank

type BankEffs = '[Bank, Error BankError]

storeKey :: BaseApp.StoreKey "bank"
storeKey = BaseApp.StoreKey "bank"

eval
  :: Members Auth.AuthEffs r
  => Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => forall a. Sem (Bank ': Error BankError ': r) a -> Sem r a
eval = mapError BaseApp.makeAppError . evalBank
  where
    evalBank
      :: Members Auth.AuthEffs r
      => Members [BaseApp.RawStore, Error BaseApp.AppError] r
      => forall a. Sem (Bank ': r) a -> Sem r a
    evalBank =
      interpret
        (\case
          GetBalance' address denomination -> do
            coins <- BaseApp.get storeKey address
            pure _
          PutBalance address balance -> do
            mAcnt <- Auth.getAccount address
            case mAcnt of
              Nothing -> do
                newAcnt <- Auth.createAccount address
                Auth.putAccount address $ newAcnt { Auth.accountCoins = balance }
              Just acnt ->
                Auth.putAccount address $ acnt { Auth.accountCoins = balance }
            BaseApp.put storeKey address balance
        )

--------------------------------------------------------------------------------

-- @TODO: make this less dumb
-- assumes only 1 coin denomination exists in a list
replaceCoinsByDenomination
  :: (Text -> Bool)
  -> Auth.Coin
  -> [Auth.Coin]
  -> [Auth.Coin]
replaceCoinsByDenomination _ coinValue [] = [coinValue]
replaceCoinsByDenomination f coinValue (x@(Auth.Coin d1 _):xs)
  | f d1 = coinValue : xs
  | otherwise = x : replaceCoinsByDenomination f coinValue xs

faucetAccount
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => FaucetAccount
  -> Sem r ()
faucetAccount FaucetAccount{..} = do
  mint faucetAccountTo faucetAccountAmount
  let event = Faucetted
        { faucettedAccount = faucetAccountTo
        , faucettedAmount = faucetAccountAmount
        }
  BaseApp.emit event
  BaseApp.logEvent event

getBalance
  :: Member Bank r
  => Address
  -> Text
  -> Sem r [Auth.Coin]
getBalance address =
  fromMaybe [] <$> getBalance' address

transfer
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => Address
  -> Auth.Coin
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
      let event = TransferEvent
            { transferEventAmount = amount
            , transferEventTo = addr2
            , transferEventFrom = addr1
            }
      BaseApp.emit event
      BaseApp.logEvent event
    else throw (InsufficientFunds "Insufficient funds for transfer.")

burn
  :: Members BankEffs r
  => Address
  -> Auth.Coin
  -> Sem r ()
burn addr amount = do
  bal <- getBalance addr
  if bal < amount
    then throw $ InsufficientFunds "Insuffient funds for burn."
    else putBalance addr (bal - amount)

mint
  :: Member Bank r
  => Address
  -> Auth.Coin
  -> Sem r ()
mint addr amount = do
  bal <- getBalance addr
  putBalance addr (bal + amount)
