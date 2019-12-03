{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Token.Keeper where

import           Data.Maybe                         (fromMaybe)
import           Nameservice.Modules.Token.Messages (FaucetAccount (..))
import           Nameservice.Modules.Token.Types    (Amount (..),
                                                     Faucetted (..),
                                                     TokenError (..),
                                                     TransferEvent (..))
import           Polysemy
import           Polysemy.Error                     (Error, mapError, throw)
import           Polysemy.Output                    (Output)
import           Polysemy.Tagged                    (Tagged)
import           Tendermint.SDK.Errors              (AppError, IsAppError (..))
import           Tendermint.SDK.Events              (Event, emit)
import qualified Tendermint.SDK.Store               as Store
import           Tendermint.SDK.Types.Address       (Address)

data Token m a where
    PutBalance :: Address -> Amount -> Token m ()
    GetBalance' :: Address -> Token m (Maybe Amount)

makeSem ''Token

type TokenEffs = '[Token, Error TokenError]

storeKey :: Store.StoreKey "token"
storeKey = Store.StoreKey "token"

eval
  :: forall (c :: Store.ConnectionScope) r.
     Members [Tagged c Store.RawStore, Error AppError, Output Event] r
  => forall a. Sem (Token ': Error TokenError ': r) a -> Sem r a
eval = mapError makeAppError . evalToken
  where
    evalToken =
      interpret
        (\case
          GetBalance' address ->
            Store.get @c storeKey address
          PutBalance address balance ->
            Store.put @c storeKey address balance
        )

--------------------------------------------------------------------------------

faucetAccount
  :: Members [Error TokenError, Output Event, Token] r
  => FaucetAccount
  -> Sem r ()
faucetAccount FaucetAccount{..} = do
  mint faucetAccountTo faucetAccountAmount
  emit Faucetted
    { faucettedAccount = faucetAccountTo
    , faucettedAmount = faucetAccountAmount
    }

getBalance
  :: Member Token r
  => Address
  -> Sem r Amount
getBalance address =
  fromMaybe (Amount 0) <$> getBalance' address

transfer
  :: Members [Error TokenError, Output Event, Token] r
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
      emit $ TransferEvent
        { transferEventAmount = amount
        , transferEventTo = addr2
        , transferEventFrom = addr1
        }
    else throw (InsufficientFunds "Insufficient funds for transfer.")

burn
  :: Members [Error TokenError, Token] r
  => Address
  -> Amount
  -> Sem r ()
burn addr amount = do
  bal <- getBalance addr
  if bal < amount
    then throw $ InsufficientFunds "Insuffient funds for burn."
    else putBalance addr (bal - amount)

mint
  :: Member Token r
  => Address
  -> Amount
  -> Sem r ()
mint addr amount = do
  bal <- getBalance addr
  putBalance addr (bal + amount)
