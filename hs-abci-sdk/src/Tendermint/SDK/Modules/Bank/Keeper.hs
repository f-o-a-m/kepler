module Tendermint.SDK.Modules.Bank.Keeper where

import           Data.List                         (find)
import           Data.Maybe                        (fromMaybe)
import qualified Data.Text                         as T
import           Polysemy
import           Polysemy.Error                    (Error, mapError, throw)
import           Polysemy.Output                   (Output)
import qualified Tendermint.SDK.BaseApp            as BaseApp
import qualified Tendermint.SDK.Modules.Auth       as Auth
import           Tendermint.SDK.Modules.Bank.Types (BankError (..),
                                                    TransferEvent (..))
import           Tendermint.SDK.Types.Address      (Address)

type BankEffs = '[Error BankError]

eval
  :: Member (Error BaseApp.AppError) r
  => forall a. Sem (Error BankError ': r) a -> Sem r a
eval = mapError BaseApp.makeAppError

--------------------------------------------------------------------------------

getCoinBalance
  :: Members Auth.AuthEffs r
  => Members BankEffs r
  => Address
  -> Auth.CoinId
  -> Sem r Auth.Coin
getCoinBalance address cid = do
  mAcnt <- Auth.getAccount address
  case mAcnt of
    Nothing -> throw . AccountDoesNotExist . T.pack . show $ address
    Just (Auth.Account coins _) -> pure $
      fromMaybe (Auth.Coin cid 0) $ find (\(Auth.Coin cid1 _) -> cid == cid1) coins

putCoinBalance
  :: Members Auth.AuthEffs r
  => Address
  -> Auth.Coin
  -> Sem r ()
putCoinBalance address coin = do
  mAcnt <- Auth.getAccount address
  case mAcnt of
    Nothing -> do
      Auth.putAccount address (Auth.Account [coin] 0)
    Just (Auth.Account coins nonce) -> do
      let updatedCoins = replaceCoinValue coin coins
      Auth.putAccount address (Auth.Account updatedCoins nonce)
  where
    replaceCoinValue c [] = [c]
    replaceCoinValue c@(Auth.Coin cid _) (c1@(Auth.Coin cid1 _):rest) =
      if cid1 == cid
      then c : rest
      else c1 : (replaceCoinValue c rest)

transfer
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members Auth.AuthEffs r
  => Members BankEffs r
  => Address
  -> Auth.Coin
  -> Address
  -> Sem r ()
transfer addr1 (Auth.Coin cid amount) addr2 = do
  -- check if addr1 has amt
  (Auth.Coin _ addr1Bal) <- getCoinBalance addr1 cid
  if addr1Bal > amount
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
    else throw (InsufficientFunds "Insufficient funds for transfer.")

burn
  :: Members Auth.AuthEffs r
  => Members BankEffs r
  => Address
  -> Auth.Coin
  -> Sem r ()
burn addr (Auth.Coin cid amount) = do
  (Auth.Coin _ bal) <- getCoinBalance addr cid
  if bal < amount
    then throw $ InsufficientFunds "Insuffient funds for burn."
    else putCoinBalance addr (Auth.Coin cid (bal - amount))

mint
  :: Members Auth.AuthEffs r
  => Members BankEffs r
  => Address
  -> Auth.Coin
  -> Sem r ()
mint addr (Auth.Coin cid amount) = do
  (Auth.Coin _ bal) <- getCoinBalance addr cid
  putCoinBalance addr (Auth.Coin cid (bal + amount))
