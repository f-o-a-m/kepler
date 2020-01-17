{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Bank.Keeper where

import           Data.List                            (find)
import           Data.Maybe                           (fromMaybe)
import           Polysemy
import           Polysemy.Error                       (Error, mapError, throw)
import           Polysemy.Output                      (Output)
import qualified Tendermint.SDK.BaseApp               as BaseApp
import qualified Tendermint.SDK.Modules.Auth          as Auth
import qualified Tendermint.SDK.Modules.Auth.Keeper   as Auth
-- import           Tendermint.SDK.Modules.Bank.Messages (FaucetAccount (..))
import           Tendermint.SDK.Modules.Bank.Types    (BankError (..),
                                                       -- Faucetted (..),
                                                       TransferEvent (..))
import           Tendermint.SDK.Types.Address         (Address)

data Bank m a where
  PutBalance :: Address -> Auth.Coin -> Bank m ()
  GetBalance' :: Address -> Auth.CoinId -> Bank m (Maybe Auth.Coin)

makeSem ''Bank

type BankEffs = '[Bank, Error BankError]

storeKey :: BaseApp.StoreKey "bank"
storeKey = BaseApp.StoreKey "bank"

eval
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => forall a. Sem (Bank ': Error BankError ': r) a -> Sem r a
eval = mapError BaseApp.makeAppError . evalBank
  where
    replaceCoinValue coin@(Auth.Coin cid _) =
      map (\c1@(Auth.Coin cid1 _) ->
             if cid1 == cid
             then coin
             else c1)

    -- @TODO: use auth over RawStore summands
    evalBank
      :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
      => forall a. Sem (Bank ': r) a -> Sem r a
    evalBank =
      interpret
        (\case
          GetBalance' address d -> do
            mAcnt <- BaseApp.get Auth.storeKey address
            case mAcnt of
              Just (Auth.Account coins _) -> do
                pure $ find (\(Auth.Coin d1 _) -> d == d1) coins
              _ -> pure Nothing
          PutBalance address coin -> do
            mAcnt <- BaseApp.get Auth.storeKey address
            case mAcnt of
              Nothing -> do
                newAcnt <- createAccount address
                BaseApp.put Auth.storeKey address $ newAcnt { Auth.accountCoins = [coin] }
              Just acnt@(Auth.Account coins _) ->
                BaseApp.put Auth.storeKey address $ acnt { Auth.accountCoins = replaceCoinValue coin coins }
        )

--------------------------------------------------------------------------------

-- @TODO: once auth effs are allowed in `eval`, delete this.
createAccount
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => Address
  -> Sem r Auth.Account
createAccount addr = do
  mAcct <- BaseApp.get Auth.storeKey addr
  case mAcct of
    -- @TODO: can't throw error here
    Just acnt -> pure acnt
    Nothing -> do
      let emptyAccount = Auth.Account
            { Auth.accountCoins = []
            , Auth.accountNonce = 0
            }
      BaseApp.put Auth.storeKey addr emptyAccount
      pure emptyAccount

-- faucetAccount
--   :: Members [BaseApp.Logger, Output BaseApp.Event] r
--   => Members BankEffs r
--   => FaucetAccount
--   -> Sem r ()
-- faucetAccount FaucetAccount{..} = do
--   let coin = Auth.Coin faucetAccountCoinId faucetAccountAmount
--   mint faucetAccountTo coin
--   let event = Faucetted
--         { faucettedAccount = faucetAccountTo
--         , faucettedCoinId = faucetAccountCoinId
--         , faucettedAmount = faucetAccountAmount
--         }
--   BaseApp.emit event
--   BaseApp.logEvent event

getBalance
  :: Member Bank r
  => Address
  -> Auth.CoinId
  -> Sem r Auth.Coin
getBalance address cid =
  fromMaybe (Auth.Coin cid 0) <$> getBalance' address cid

transfer
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => Address
  -> Auth.Coin
  -> Address
  -> Sem r ()
transfer addr1 (Auth.Coin cid amount) addr2 = do
  -- check if addr1 has amt
  (Auth.Coin _ addr1Bal) <- getBalance addr1 cid
  if addr1Bal > amount
    then do
      (Auth.Coin _ addr2Bal) <- getBalance addr2 cid
      let newCoinBalance1 = Auth.Coin cid (addr1Bal - amount)
          newCoinBalance2 = Auth.Coin cid (addr2Bal + amount)
      -- update both balances
      putBalance addr1 newCoinBalance1
      putBalance addr2 newCoinBalance2
      let event = TransferEvent
            { transferEventAmount = amount
            , transferEventCoinId = cid
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
burn addr (Auth.Coin cid amount) = do
  (Auth.Coin _ bal) <- getBalance addr cid
  if bal < amount
    then throw $ InsufficientFunds "Insuffient funds for burn."
    else putBalance addr (Auth.Coin cid (bal - amount))

mint
  :: Member Bank r
  => Address
  -> Auth.Coin
  -> Sem r ()
mint addr (Auth.Coin cid amount) = do
  (Auth.Coin _ bal) <- getBalance addr cid
  putBalance addr (Auth.Coin cid (bal + amount))
