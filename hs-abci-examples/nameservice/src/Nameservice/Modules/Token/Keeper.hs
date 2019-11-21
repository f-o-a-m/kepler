{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Token.Keeper where

import qualified Tendermint.SDK.Store            as Store
import           Nameservice.Modules.Token.Types (Amount(..), TokenException(..), Transfer(..))
import           Tendermint.SDK.Types.Address    (Address)
import           Polysemy
import           Polysemy.Error                  (Error, mapError, throw)
import           Polysemy.Output                 (Output)
import           Tendermint.SDK.BaseApp          (HasBaseAppEff)
import           Tendermint.SDK.Events           (Event, emit)
import           Tendermint.SDK.Errors           (IsAppError (..))
import           Data.Maybe                      (fromMaybe)


data Token m a where
    PutBalance :: Address -> Amount -> Token m ()
    GetBalance' :: Address -> Token m (Maybe Amount)

makeSem ''Token

type TokenEffR = '[Token, Error TokenException]
type HasTokenEff r = (Members TokenEffR r, Member (Output Event) r)

storeKey :: Store.StoreKey "token"
storeKey = Store.StoreKey "token"

eval
  :: HasBaseAppEff r
  => Sem (Token ': Error TokenException ': r) a
  -> Sem r a
eval = mapError makeAppError . evalToken
  where
    evalToken
      :: HasBaseAppEff r
      => Sem (Token ': r) a
      -> Sem r a
    evalToken =
      interpret (\case
                    GetBalance' address ->
                      Store.get storeKey address
                    PutBalance address balance ->
                      Store.put storeKey address balance
                )

--------------------------------------------------------------------------------

getBalance
  :: Member Token r
  => Address
  -> Sem r Amount
getBalance address =
  fromMaybe (Amount 0) <$> getBalance' address

transfer
  :: HasTokenEff r
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
      emit $ Transfer
        { transferAmount = amount
        , transferTo = addr2
        , transferFrom = addr1
        }
    else throw (InsufficientFunds "Insufficient funds for transfer.")

burn
  :: Members '[Token, Error TokenException] r
  => Address
  -> Amount
  -> Sem r ()
burn addr amount = do
  bal <- getBalance addr
  if bal < amount
    then throw $ InsufficientFunds "Insuffient funds for burn."
    else putBalance addr (bal - amount)

mint
  :: Members '[Token, Error TokenException] r
  => Address
  -> Amount
  -> Sem r ()
mint addr amount = do
  bal <- getBalance addr
  putBalance addr (bal + amount)
