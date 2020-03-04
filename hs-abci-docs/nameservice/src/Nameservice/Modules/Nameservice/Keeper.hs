{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Nameservice.Keeper
  ( NameserviceEffs
  , NameserviceKeeper(..)
  , nameserviceCoinId
  , setName
  , deleteName
  , buyName
  , faucetAccount
  , getWhois
  , eval
  ) where

import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Store
import           Nameservice.Modules.Nameservice.Types
import           Polysemy                                 (Member, Members, Sem,
                                                           interpret, makeSem)
import           Polysemy.Error                           (Error, mapError,
                                                           throw)
import           Polysemy.Output                          (Output)
import qualified Tendermint.SDK.BaseApp                   as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Map         as M
import           Tendermint.SDK.Modules.Auth              (Coin (..), CoinId)
import           Tendermint.SDK.Modules.Bank              (BankEffs, burn, mint,
                                                           transfer)

data NameserviceKeeper m a where
  FaucetAccount :: FaucetAccountMsg -> NameserviceKeeper m ()
  BuyName :: BuyNameMsg -> NameserviceKeeper m ()
  DeleteName :: DeleteNameMsg -> NameserviceKeeper m ()
  SetName :: SetNameMsg -> NameserviceKeeper m ()
  GetWhois :: Name -> NameserviceKeeper m (Maybe Whois)

makeSem ''NameserviceKeeper

type NameserviceEffs = '[NameserviceKeeper, Error NameserviceError]

nameserviceCoinId :: CoinId
nameserviceCoinId = "nameservice"

eval
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => forall a. Sem (NameserviceKeeper ': Error NameserviceError ': r) a
  -> Sem r a
eval = mapError BaseApp.makeAppError . evalNameservice
  where
    evalNameservice
      :: Members BaseApp.TxEffs r
      => Members BaseApp.BaseEffs r
      => Members BankEffs r
      => Member (Error NameserviceError) r
      => Sem (NameserviceKeeper ': r) a -> Sem r a
    evalNameservice =
      interpret (\case
          FaucetAccount msg -> faucetAccountF msg
          BuyName msg -> buyNameF msg
          DeleteName msg -> deleteNameF msg
          SetName msg -> setNameF msg
          GetWhois name -> M.lookup name whoisMap
        )

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

faucetAccountF
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => FaucetAccountMsg
  -> Sem r ()
faucetAccountF FaucetAccountMsg{..} = do
  let coin = Coin faucetAccountCoinId faucetAccountAmount
  mint faucetAccountTo coin
  let event = Faucetted
        { faucettedAccount = faucetAccountTo
        , faucettedCoinId = faucetAccountCoinId
        , faucettedAmount = faucetAccountAmount
        }
  BaseApp.emit event
  BaseApp.logEvent event

setNameF
  :: Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error NameserviceError) r
  => SetNameMsg
  -> Sem r ()
setNameF SetNameMsg{..} = do
  mwhois <- M.lookup (Name setNameName) whoisMap
  case mwhois of
    Nothing -> throw $ UnauthorizedSet "Cannot claim name with SetMessage tx."
    Just currentWhois@Whois{..} ->
      if whoisOwner /= setNameOwner
        then throw $ UnauthorizedSet "Setter must be the owner of the Name."
        else do
          M.insert (Name setNameName) (currentWhois {whoisValue = setNameValue}) whoisMap
          let event = NameRemapped
                { nameRemappedName = setNameName
                , nameRemappedNewValue = setNameValue
                , nameRemappedOldValue = whoisValue
                }
          BaseApp.emit event
          BaseApp.logEvent event

deleteNameF
  :: Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => Members BankEffs r
  => Member (Error NameserviceError) r
  => DeleteNameMsg
  -> Sem r ()
deleteNameF DeleteNameMsg{..} = do
  mWhois <- M.lookup (Name deleteNameName) whoisMap
  case mWhois of
    Nothing -> throw $ InvalidDelete "Can't remove unassigned name."
    Just Whois{..} ->
      if whoisOwner /= deleteNameOwner
        then throw $ InvalidDelete "Deleter must be the owner."
        else do
          mint deleteNameOwner (Coin nameserviceCoinId whoisPrice)
          M.delete (Name deleteNameName) whoisMap
          let event = NameDeleted
                { nameDeletedName = deleteNameName
                }
          BaseApp.emit event
          BaseApp.logEvent event

buyNameF
  :: Members BaseApp.TxEffs r
  => Members BankEffs r
  => Members BaseApp.BaseEffs r
  => Member (Error NameserviceError) r
  => BuyNameMsg
  -> Sem r ()
-- ^ did it succeed
buyNameF msg = do
  let name = buyNameName msg
  mWhois <- M.lookup (Name name) whoisMap
  case mWhois of
    -- The name is unclaimed, go ahead and debit the account
    -- and create it.
    Nothing    -> buyUnclaimedName msg
    -- The name is currently claimed, we will transfer the
    -- funds and ownership
    Just whois -> buyClaimedName msg whois
    where
      buyUnclaimedName
        :: Members BaseApp.TxEffs r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Sem r ()
      buyUnclaimedName BuyNameMsg{..} = do
        burn buyNameBuyer (Coin nameserviceCoinId buyNameBid)
        let whois = Whois
              { whoisOwner = buyNameBuyer
              , whoisValue = buyNameValue
              , whoisPrice = buyNameBid
              }
        M.insert (Name buyNameName) whois whoisMap
        let event = NameClaimed
              { nameClaimedOwner = buyNameBuyer
              , nameClaimedName = buyNameName
              , nameClaimedValue = buyNameValue
              , nameClaimedBid = buyNameBid
              }
        BaseApp.emit event
        BaseApp.logEvent event

      buyClaimedName
        :: Members BaseApp.TxEffs r
        => Member (Error NameserviceError) r
        => Members BaseApp.BaseEffs r
        => Members BankEffs r
        => BuyNameMsg
        -> Whois
        -> Sem r ()
      buyClaimedName BuyNameMsg{..} currentWhois =
        let Whois{ whoisPrice = forsalePrice, whoisOwner = previousOwner } = currentWhois
        in if buyNameBid > forsalePrice
             then do
               transfer buyNameBuyer (Coin nameserviceCoinId buyNameBid) previousOwner
               -- update new owner, price and value based on BuyName
               let whois' = currentWhois
                     { whoisOwner = buyNameBuyer
                     , whoisPrice = buyNameBid
                     , whoisValue = buyNameValue
                     }
               M.insert (Name buyNameName) whois' whoisMap
               let event = NameClaimed
                     { nameClaimedOwner = buyNameBuyer
                     , nameClaimedName = buyNameName
                     , nameClaimedValue = buyNameValue
                     , nameClaimedBid = buyNameBid
                     }
               BaseApp.emit event
               BaseApp.logEvent event
             else throw (InsufficientBid "Bid must exceed the price.")

