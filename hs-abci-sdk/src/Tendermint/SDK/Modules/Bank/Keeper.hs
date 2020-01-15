{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Bank.Keeper where

import           Data.List                            (find)
import           Data.Maybe                           (fromMaybe)
import           Data.Text                            (Text)
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

data Bank m a where
    PutBalance :: Address -> Auth.Coin -> Bank m ()
    GetBalance' :: Address -> Text -> Bank m (Maybe Auth.Coin)

makeSem ''Bank

type BankEffs = '[Bank, Error BankError]

eval
  :: Members Auth.AuthEffs r
  => Member (Error BaseApp.AppError) r
  => forall a. Sem (Bank ': Error BankError ': r) a -> Sem r a
eval = mapError BaseApp.makeAppError . evalBank
  where
    replaceCoinValue coin@(Auth.Coin denom _) =
      map (\c1@(Auth.Coin d1 _) ->
             if d1 == denom
             then coin
             else c1)
    evalBank
      :: Members Auth.AuthEffs r
      => forall a. Sem (Bank ': r) a -> Sem r a
    evalBank =
      interpret
        (\case
          GetBalance' address d -> do
            mAcnt <- Auth.getAccount address
            case mAcnt of
              Just (Auth.Account coins _) -> do
                pure $ find (\(Auth.Coin d1 _) -> d == d1) coins
              _ -> pure Nothing
          PutBalance address coin -> do
            mAcnt <- Auth.getAccount address
            case mAcnt of
              Nothing -> do
                newAcnt <- Auth.createAccount address
                Auth.putAccount address $ newAcnt { Auth.accountCoins = [coin] }
              Just acnt@(Auth.Account coins _) ->
                Auth.putAccount address $ acnt { Auth.accountCoins = replaceCoinValue coin coins }
        )

--------------------------------------------------------------------------------

faucetAccount
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => FaucetAccount
  -> Sem r ()
faucetAccount FaucetAccount{..} = do
  mint faucetAccountTo (Auth.Coin faucetAccountDenomination faucetAccountAmount)
  let event = Faucetted
        { faucettedAccount = faucetAccountTo
        , faucettedDenomination = faucetAccountDenomination
        , faucettedAmount = faucetAccountAmount
        }
  BaseApp.emit event
  BaseApp.logEvent event

getBalance
  :: Member Bank r
  => Address
  -> Text
  -> Sem r Auth.Coin
getBalance address denom =
  fromMaybe (Auth.Coin denom 0) <$> getBalance' address denom

transfer
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => Address
  -> Auth.Coin
  -> Address
  -> Sem r ()
transfer addr1 (Auth.Coin denom amount) addr2 = do
  -- check if addr1 has amt
  (Auth.Coin _ addr1Bal) <- getBalance addr1 denom
  if addr1Bal > amount
    then do
      (Auth.Coin _ addr2Bal) <- getBalance addr2 denom
      let newCoinBalance1 = Auth.Coin denom (addr1Bal - amount)
          newCoinBalance2 = Auth.Coin denom (addr2Bal + amount)
      -- update both balances
      putBalance addr1 newCoinBalance1
      putBalance addr2 newCoinBalance2
      let event = TransferEvent
            { transferEventAmount = amount
            , transferEventDenomination = denom
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
burn addr (Auth.Coin denom amount) = do
  (Auth.Coin _ bal) <- getBalance addr denom
  if bal < amount
    then throw $ InsufficientFunds "Insuffient funds for burn."
    else putBalance addr (Auth.Coin denom (bal - amount))

mint
  :: Member Bank r
  => Address
  -> Auth.Coin
  -> Sem r ()
mint addr (Auth.Coin denom amount) = do
  (Auth.Coin _ bal) <- getBalance addr denom
  putBalance addr (Auth.Coin denom (bal + amount))
