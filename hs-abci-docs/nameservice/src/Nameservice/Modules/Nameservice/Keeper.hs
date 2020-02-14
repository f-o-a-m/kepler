{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Nameservice.Keeper
  ( NameserviceKeeper
  , NameserviceEffs
  , setName
  , deleteName
  , buyName
  , storeKey
  , eval
  ) where

import           Data.Proxy
import           Data.String.Conversions                  (cs)
import           GHC.TypeLits                             (symbolVal)
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Types
import           Nameservice.Modules.Token                (Token, TokenEffs,
                                                           burn, mint, transfer)
import           Polysemy                                 (Members, Sem,
                                                           interpret, makeSem)
import           Polysemy.Error                           (Error, mapError,
                                                           throw)
import           Polysemy.Output                          (Output)
import qualified Tendermint.SDK.BaseApp                   as BaseApp

data NameserviceKeeper m a where
  PutWhois :: Name -> Whois -> NameserviceKeeper m ()
  GetWhois :: Name -> NameserviceKeeper m (Maybe Whois)
  DeleteWhois :: Name -> NameserviceKeeper m ()

makeSem ''NameserviceKeeper

type NameserviceEffs = '[NameserviceKeeper, Error NameserviceError]

storeKey :: BaseApp.StoreKey NameserviceModuleName
storeKey = BaseApp.StoreKey . cs . symbolVal $ Proxy @NameserviceModuleName

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
  :: Members [BaseApp.Logger, Token, Output BaseApp.Event] r
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
          mint deleteNameOwner whoisPrice
          deleteWhois deleteNameName
          let event = NameDeleted
                { nameDeletedName = deleteNameName
                }
          BaseApp.emit event
          BaseApp.logEvent event

buyName
  :: Members [BaseApp.Logger, Output BaseApp.Event] r
  => Members TokenEffs r
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
        => Members TokenEffs r
        => Members NameserviceEffs r
        => BuyName
        -> Sem r ()
      buyUnclaimedName BuyName{..} = do
        burn buyNameBuyer buyNameBid
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
        => Members TokenEffs r
        => Members [BaseApp.Logger, Output BaseApp.Event] r
        => BuyName
        -> Whois
        -> Sem r ()
      buyClaimedName BuyName{..} currentWhois =
        let Whois{ whoisPrice = forsalePrice, whoisOwner = previousOwner } = currentWhois
        in if buyNameBid > forsalePrice
             then do
               transfer buyNameBuyer buyNameBid previousOwner
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

