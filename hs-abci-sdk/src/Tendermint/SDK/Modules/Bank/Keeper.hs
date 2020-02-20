{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Bank.Keeper where

import           Data.List                         (find)
import           Data.Maybe                        (fromMaybe)
import           Polysemy
import           Polysemy.Error                    (Error, mapError, throw)
import           Polysemy.Output                   (Output)
import qualified Tendermint.SDK.BaseApp            as BaseApp
import qualified Tendermint.SDK.Modules.Auth       as Auth
import           Tendermint.SDK.Modules.Bank.Types (BankError (..),
                                                    TransferEvent (..))
import           Tendermint.SDK.Types.Address      (Address)

type BankEffs = '[BankKeeper, Error BankError]

data BankKeeper m a where
  GetBalance :: Address -> Auth.CoinId -> BankKeeper m Auth.Coin
  PutBalance :: Address -> Auth.Coin -> BankKeeper m ()

makeSem ''BankKeeper

eval
  :: Member (Error BaseApp.AppError) r
  => Members Auth.AuthEffs r
  => forall a. Sem (BankKeeper ': Error BankError ': r) a -> Sem r a
eval = mapError BaseApp.makeAppError . evalBankKeeper
  where
    evalBankKeeper
      :: forall r.
         Members Auth.AuthEffs r
      => forall a.
         Sem (BankKeeper ': r) a
      -> Sem r a
    evalBankKeeper = interpret (\case
      GetBalance addr coinId -> getCoinBalance addr coinId
      PutBalance addr coins -> putCoinBalance addr coins
      )

    getCoinBalance
      :: Members Auth.AuthEffs r
      => Address
      -> Auth.CoinId
      -> Sem r Auth.Coin
    getCoinBalance address cid = do
      mAcnt <- Auth.getAccount address
      let zeroBalance = Auth.Coin cid 0
      case mAcnt of
        Nothing -> pure zeroBalance
        Just (Auth.Account coins _) ->
          let mCoin = find (\(Auth.Coin cid1 _) -> cid == cid1) coins
          in pure $ fromMaybe zeroBalance mCoin

    replaceCoinValue
      :: Auth.Coin
      -> [Auth.Coin]
      -> [Auth.Coin]
    replaceCoinValue c [] = [c]
    replaceCoinValue c@(Auth.Coin cid _) (c1@(Auth.Coin cid' _):rest) =
      if cid' == cid
      then c : rest
      else c1 : replaceCoinValue c rest

    putCoinBalance
      :: Members Auth.AuthEffs r
      => Address
      -> Auth.Coin
      -> Sem r ()
    putCoinBalance address coin = do
      mAcnt <- Auth.getAccount address
      acnt <- case mAcnt of
                Nothing -> Auth.createAccount address
                Just a  -> pure a
      let updatedCoins = replaceCoinValue coin (Auth.accountCoins acnt)
          updatedAcnt = acnt { Auth.accountCoins = updatedCoins }
      Auth.putAccount address updatedAcnt

--------------------------------------------------------------------------------

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
  if addr1Bal >= amount
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
    then throw $ InsufficientFunds "Insufficient funds for burn."
    else putBalance addr (Auth.Coin cid (bal - amount))

mint
  :: Member BankKeeper r
  => Address
  -> Auth.Coin
  -> Sem r ()
mint addr (Auth.Coin cid amount) = do
  (Auth.Coin _ bal) <- getBalance addr cid
  putBalance addr (Auth.Coin cid (bal + amount))
