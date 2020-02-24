{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Nameservice.Keeper
  ( NameserviceKeeper
  , NameserviceEffs
  , nameserviceCoinId
  , setName
  , deleteName
  , buyName
  , storeKey
  , faucetAccount
  , eval
  ) where

import           Data.Proxy
import           Data.String.Conversions                  (cs)
import           GHC.TypeLits                             (symbolVal)
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Types
import           Polysemy                                 (Members, Sem,
                                                           interpret, makeSem)
import           Polysemy.Error                           (Error, mapError,
                                                           throw)
import           Polysemy.Output                          (Output)
import qualified Tendermint.SDK.BaseApp                   as BaseApp
import           Tendermint.SDK.Modules.Auth              (Coin (..), CoinId)
import           Tendermint.SDK.Modules.Bank              (BankEffs, burn, mint,
                                                           transfer)

data NameserviceKeeper m a where
  PutWhois :: Name -> Whois -> NameserviceKeeper m ()
  GetWhois :: Name -> NameserviceKeeper m (Maybe Whois)
  DeleteWhois :: Name -> NameserviceKeeper m ()

makeSem ''NameserviceKeeper

type NameserviceEffs = '[NameserviceKeeper, Error NameserviceError]

storeKey :: BaseApp.StoreKey NameserviceNamespace
storeKey = BaseApp.StoreKey . cs . symbolVal $ Proxy @NameserviceName

nameserviceCoinId :: CoinId
nameserviceCoinId = "nameservice"

eval
  :: Members BaseApp.TxEffs r
  => forall a. Sem (NameserviceKeeper ': Error NameserviceError ': r) a
  -> Sem r a
eval = mapError BaseApp.makeAppError . evalNameservice
  where
    evalNameservice
      :: Members BaseApp.TxEffs r
      => Sem (NameserviceKeeper ': r) a -> Sem r a
    evalNameservice =
      interpret (\case
          GetWhois name ->
            BaseApp.get storeKey name
          PutWhois name whois ->
            BaseApp.put storeKey name whois
          DeleteWhois name ->
            BaseApp.delete storeKey name
        )

--------------------------------------------------------------------------------

faucetAccount
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => FaucetAccount
  -> Sem r ()
faucetAccount FaucetAccount{..} = do
  let coin = Coin faucetAccountCoinId faucetAccountAmount
  mint faucetAccountTo coin
  let event = Faucetted
        { faucettedAccount = faucetAccountTo
        , faucettedCoinId = faucetAccountCoinId
        , faucettedAmount = faucetAccountAmount
        }
  BaseApp.emit event
  BaseApp.logEvent event

setName
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members NameserviceEffs r
  => SetName
  -> Sem r ()
setName SetName{..} = do
  mwhois <- getWhois setNameName
  case mwhois of
    Nothing -> throw $ UnauthorizedSet "Cannot claim name with SetMessage tx."
    Just currentWhois@Whois{..} ->
      if whoisOwner /= setNameOwner
        then throw $ UnauthorizedSet "Setter must be the owner of the Name."
        else do
          putWhois setNameName currentWhois {whoisValue = setNameValue}
          let event = NameRemapped
                { nameRemappedName = setNameName
                , nameRemappedNewValue = setNameValue
                , nameRemappedOldValue = whoisValue
                }
          BaseApp.emit event
          BaseApp.logEvent event

deleteName
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => Members NameserviceEffs r
  => DeleteName
  -> Sem r ()
deleteName DeleteName{..} = do
  mWhois <- getWhois deleteNameName
  case mWhois of
    Nothing -> throw $ InvalidDelete "Can't remove unassigned name."
    Just Whois{..} ->
      if whoisOwner /= deleteNameOwner
        then throw $ InvalidDelete "Deleter must be the owner."
        else do
          mint deleteNameOwner (Coin nameserviceCoinId whoisPrice)
          deleteWhois deleteNameName
          let event = NameDeleted
                { nameDeletedName = deleteNameName
                }
          BaseApp.emit event
          BaseApp.logEvent event

buyName
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members BankEffs r
  => Members NameserviceEffs r
  => BuyName
  -> Sem r ()
-- ^ did it succeed
buyName msg = do
  let name = buyNameName msg
  mWhois <- getWhois name
  case mWhois of
    -- The name is unclaimed, go ahead and debit the account
    -- and create it.
    Nothing    -> buyUnclaimedName msg
    -- The name is currently claimed, we will transfer the
    -- funds and ownership
    Just whois -> buyClaimedName msg whois
    where
      buyUnclaimedName
        :: Members [BaseApp.Logger, Output BaseApp.Event] r
        => Members BankEffs r
        => Members NameserviceEffs r
        => BuyName
        -> Sem r ()
      buyUnclaimedName BuyName{..} = do
        burn buyNameBuyer (Coin nameserviceCoinId buyNameBid)
        let whois = Whois
              { whoisOwner = buyNameBuyer
              , whoisValue = buyNameValue
              , whoisPrice = buyNameBid
              }
        putWhois buyNameName whois
        let event = NameClaimed
              { nameClaimedOwner = buyNameBuyer
              , nameClaimedName = buyNameName
              , nameClaimedValue = buyNameValue
              , nameClaimedBid = buyNameBid
              }
        BaseApp.emit event
        BaseApp.logEvent event

      buyClaimedName
        :: Members NameserviceEffs r
        => Members BankEffs r
        => Members [BaseApp.Logger, Output BaseApp.Event] r
        => BuyName
        -> Whois
        -> Sem r ()
      buyClaimedName BuyName{..} currentWhois =
        let Whois{ whoisPrice = forsalePrice, whoisOwner = previousOwner } = currentWhois
        in if buyNameBid > forsalePrice
             then do
               transfer buyNameBuyer (Coin nameserviceCoinId buyNameBid) previousOwner
               -- update new owner, price and value based on BuyName
               putWhois buyNameName currentWhois { whoisOwner = buyNameBuyer
                                                 , whoisPrice = buyNameBid
                                                 , whoisValue = buyNameValue
                                                 }
               let event = NameClaimed
                     { nameClaimedOwner = buyNameBuyer
                     , nameClaimedName = buyNameName
                     , nameClaimedValue = buyNameValue
                     , nameClaimedBid = buyNameBid
                     }
               BaseApp.emit event
               BaseApp.logEvent event
             else throw (InsufficientBid "Bid must exceed the price.")

