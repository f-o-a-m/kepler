{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Token.Keeper where

import           Data.Maybe                            (fromMaybe)
import           Polysemy
import           Polysemy.Error                        (Error, mapError, throw)
import           Polysemy.Output                       (Output)
import qualified Tendermint.SDK.BaseApp                as BaseApp
import           Tendermint.SDK.Modules.Token.Messages (FaucetAccount (..))
import           Tendermint.SDK.Modules.Token.Types    (Amount (..),
                                                        Faucetted (..),
                                                        TokenError (..),
                                                        TransferEvent (..))
import           Tendermint.SDK.Types.Address          (Address)

data Token m a where
    PutBalance :: Address -> Amount -> Token m ()
    GetBalance' :: Address -> Token m (Maybe Amount)

makeSem ''Token

type TokenEffs = '[Token, Error TokenError]

storeKey :: BaseApp.StoreKey "token"
storeKey = BaseApp.StoreKey "token"

eval
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => forall a. Sem (Token ': Error TokenError ': r) a -> Sem r a
eval = mapError BaseApp.makeAppError . evalToken
  where
    evalToken
      :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
      => forall a. Sem (Token ': r) a -> Sem r a
    evalToken =
      interpret
        (\case
          GetBalance' address ->
            BaseApp.get storeKey address
          PutBalance address balance ->
            BaseApp.put storeKey address balance
        )

--------------------------------------------------------------------------------

faucetAccount
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members TokenEffs r
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
  :: Member Token r
  => Address
  -> Sem r Amount
getBalance address =
  fromMaybe (Amount 0) <$> getBalance' address

transfer
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members TokenEffs r
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
      let event = TransferEvent
            { transferEventAmount = amount
            , transferEventTo = addr2
            , transferEventFrom = addr1
            }
      BaseApp.emit event
      BaseApp.logEvent event
    else throw (InsufficientFunds "Insufficient funds for transfer.")

burn
  :: Members TokenEffs r
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
