{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Bank.Keeper
  ( BankEffs
  , BankKeeper(..)
  , getBalance
  , transfer
  , burn
  , mint
  , eval
  , endBlockF
  ) where

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
  Transfer :: Address -> Auth.Coin -> Address -> BankKeeper m ()
  Burn :: Address -> Auth.Coin -> BankKeeper m ()
  Mint :: Address -> Auth.Coin -> BankKeeper m ()

makeSem ''BankKeeper

eval
  :: Members [BaseApp.Logger, Output BaseApp.Event, Error BaseApp.AppError] r
  => Members Auth.AuthEffs r
  => forall a. Sem (BankKeeper ': Error BankError ': r) a -> Sem r a
eval = mapError BaseApp.makeAppError . evalBankKeeper
  where
    evalBankKeeper
      :: forall r.
         Members Auth.AuthEffs r
      => Members [BaseApp.Logger, Output BaseApp.Event, Error BankError ] r
      => forall a.
         Sem (BankKeeper ': r) a
      -> Sem r a
    evalBankKeeper = interpret (\case
      GetBalance addr coinId -> getCoinBalance addr coinId
      Transfer from coin to -> transferF from coin to
      Burn addr coin -> burnF addr coin
      Mint addr coin -> mintF addr coin
      )

--------------------------------------------------------------------------------

transferF
  :: Members [BaseApp.Logger, Output BaseApp.Event, Error BankError] r
  => Members Auth.AuthEffs r
  => Address
  -> Auth.Coin
  -> Address
  -> Sem r ()
transferF addr1 (Auth.Coin cid amount) addr2 = do
  -- check if addr1 has amt
  (Auth.Coin _ addr1Bal) <- getCoinBalance addr1 cid
  if addr1Bal >= amount
    then do
      (Auth.Coin _ addr2Bal) <- getCoinBalance addr2 cid
      let newCoinBalance1 = Auth.Coin cid (addr1Bal - amount)
          newCoinBalance2 = Auth.Coin cid (addr2Bal + amount)
      -- update both balances
      putCoinBalance addr1 newCoinBalance1
      putCoinBalance addr2 newCoinBalance2
      let event = TransferEvent
            { transferEventAmount = amount
            , transferEventCoinId = cid
            , transferEventTo = addr2
            , transferEventFrom = addr1
            }
      BaseApp.emit event
      BaseApp.logEvent event
    else throw @BankError (InsufficientFunds "Insufficient funds for transfer.")

burnF
  :: Members Auth.AuthEffs r
  => Member (Error BankError) r
  => Address
  -> Auth.Coin
  -> Sem r ()
burnF addr (Auth.Coin cid amount) = do
  (Auth.Coin _ bal) <- getCoinBalance addr cid
  if bal < amount
    then throw @BankError $ InsufficientFunds "Insufficient funds for burn."
    else putCoinBalance addr (Auth.Coin cid (bal - amount))

mintF
  :: Members Auth.AuthEffs r
  => Address
  -> Auth.Coin
  -> Sem r ()
mintF addr (Auth.Coin cid amount) = do
  (Auth.Coin _ bal) <- getCoinBalance addr cid
  putCoinBalance addr (Auth.Coin cid (bal + amount))

endBlockF
  :: Members Auth.AuthEffs r
  => Member (Error BankError) r
  => Sem r ()
endBlockF = do
  _ <- throw @Auth.AuthError undefined
  throw @BankError undefined

--------------------------------------------------------------------------------

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
  let f a = a { Auth.accountCoins = replaceCoinValue coin $ Auth.accountCoins acnt }
  Auth.updateAccount address f

