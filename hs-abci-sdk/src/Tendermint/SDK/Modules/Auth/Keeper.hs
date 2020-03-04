{-# LANGUAGE TemplateHaskell #-}

module Tendermint.SDK.Modules.Auth.Keeper
  ( AuthEffs
  , Accounts(..)
  , createAccount
  , updateAccount
  , getAccount
  , eval
  -- stores
  , accountsMap
  ) where

import           Control.Lens                      (iso)
import           Polysemy
import           Polysemy.Error                    (Error, mapError, throw)
import           Tendermint.SDK.BaseApp            (AppError, IsKey (..),
                                                    KeyRoot (..), RawKey (..),
                                                    ReadStore, Store,
                                                    WriteStore, makeAppError,
                                                    makeStore)
import qualified Tendermint.SDK.BaseApp.Store.Map  as M
import           Tendermint.SDK.Modules.Auth.Types
import           Tendermint.SDK.Types.Address      (Address)

data Accounts m a where
  CreateAccount :: Address -> Accounts m Account
  UpdateAccount :: Address -> (Account -> Account) -> Accounts m ()
  GetAccount :: Address -> Accounts m (Maybe Account)

makeSem ''Accounts

type AuthEffs = '[Accounts, Error AuthError]

eval
  :: Members [ReadStore, WriteStore, Error AppError] r
  => Sem (Accounts : Error AuthError : r) a
  -> Sem r a
eval = mapError makeAppError . evalAuth
  where
    evalAuth :: Members [ReadStore, WriteStore, Error AuthError, Error AppError] r
             => Sem (Accounts : r) a
             -> Sem r a
    evalAuth =
      interpret (\case
          CreateAccount addr -> createAccountF addr
          UpdateAccount addr f -> updateAccountF addr f
          GetAccount addr -> getAccountF addr
        )

--------------------------------------------------------------------------------

data AuthNamespace

store :: Store AuthNamespace
store = makeStore $ KeyRoot "auth"

data AccountsMapKey = AccountsMapKey

instance RawKey AccountsMapKey where
    rawKey = iso (const "accounts") (const AccountsMapKey)

instance IsKey AccountsMapKey AuthNamespace where
  type Value AccountsMapKey AuthNamespace = M.Map Address Account

accountsMap :: M.Map Address Account
accountsMap = M.makeMap AccountsMapKey store

createAccountF
  :: Members [ReadStore, WriteStore, Error AppError, Error AuthError] r
  => Address
  -> Sem r Account
createAccountF addr = do
  mAcct <- M.lookup addr accountsMap
  case mAcct of
    Just _ -> throw $ AccountAlreadyExists addr
    Nothing -> do
      let emptyAccount = Account
            { accountCoins = []
            , accountNonce = 0
            }
      M.insert addr emptyAccount accountsMap
      pure emptyAccount

updateAccountF
  :: Members [ReadStore, WriteStore, Error AppError, Error AuthError] r
  => Address
  -> (Account -> Account)
  -> Sem r ()
updateAccountF addr f = do
  mAcct <- M.lookup addr accountsMap
  case mAcct of
    Nothing   -> throw $ AccountNotFound addr
    Just acct -> M.insert addr (f acct) accountsMap

getAccountF
  :: Members [ReadStore, Error AppError] r
  => Address
  -> Sem r (Maybe Account)
getAccountF addr = M.lookup addr accountsMap
