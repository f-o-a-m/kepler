{-# LANGUAGE TemplateHaskell #-}

module Nameservice.Modules.Nameservice.Keeper where

import           Data.Maybe                               (isNothing)
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Types
import           Nameservice.Modules.Token                (Address, Amount,
                                                           HasTokenEff, burn,
                                                           mint, transfer)
import           Polysemy                                 (Member, Members, Sem,
                                                           interpret, makeSem)
import           Polysemy.Error                           (Error, mapError,
                                                           throw)
import           Polysemy.Output                          (Output)
import           Tendermint.SDK.BaseApp                   (HasBaseApp)
import           Tendermint.SDK.Errors                    (AppError (..),
                                                           IsAppError (..))
import           Tendermint.SDK.Events                    (Event, emit)
import qualified Tendermint.SDK.Store                     as Store

data Nameservice m a where
  PutWhois :: Name -> Whois -> Nameservice m ()
  GetWhois :: Name -> Nameservice m (Maybe Whois)
  DeleteWhois :: Name -> Nameservice m ()

makeSem ''Nameservice

type NameserviceEffR = '[Nameservice, Error NameserviceException]
type HasNameserviceEff r = Members NameserviceEffR r

eval
  :: HasBaseApp r
  => HasTokenEff r
  => Member (Error AppError) r
  => Sem (Nameservice ': Error NameserviceException ': r) a
  -> Sem r a
eval = mapError makeAppError . evalNameservice
  where
    evalNameservice
      :: HasBaseApp r
      => Sem (Nameservice ': r) a
      -> Sem r a
    evalNameservice =
      interpret (\case
          GetWhois name ->
            Store.get (undefined :: Store.Root) name
          PutWhois name whois ->
            Store.put name whois
          DeleteWhois name ->
            Store.delete (undefined :: Store.Root) name
        )

--------------------------------------------------------------------------------

setName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => MsgSetName
  -> Sem r ()
setName MsgSetName{..} = do
  mwhois <- getWhois msgSetNameName
  case mwhois of
    Nothing -> throw $ UnauthorizedSet "Cannot claim name with SetMessage tx."
    Just currentWhois@Whois{..} ->
      if whoisOwner /= msgSetNameOwner
        then throw $ UnauthorizedSet "Setter must be the owner of the Name."
        else do
          putWhois msgSetNameName currentWhois {whoisValue = msgSetNameValue}
          emit NameRemapped
             { nameRemappedName = msgSetNameName
             , nameRemappedNewValue = msgSetNameValue
             , nameRemappedOldValue = whoisValue
             }

deleteName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => MsgDeleteName
  -> Sem r ()
deleteName MsgDeleteName{..} = do
  mWhois <- getWhois msgDeleteNameName
  case mWhois of
    Nothing -> throw $ InvalidDelete "Can't remove unassigned name."
    Just Whois{..} ->
      if whoisOwner /= msgDeleteNameOwner
        then throw $ InvalidDelete "Deleter must be the owner."
        else do
          mint msgDeleteNameOwner whoisPrice
          deleteWhois msgDeleteNameName
          emit NameDeleted
            { nameDeletedName = msgDeleteNameName
            }


buyName
  :: HasTokenEff r
  => HasNameserviceEff r
  => Member (Output Event) r
  => MsgBuyName
  -> Sem r ()
-- ^ did it succeed
buyName msg@MsgBuyName{..} = do
  let name =  msgBuyNameName
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
        :: HasTokenEff r
        => HasNameserviceEff r
        => Member (Output Event) r
        => MsgBuyName
        -> Sem r ()
      buyUnclaimedName MsgBuyName{..} = do
        burn msgBuyNameBuyer msgBuyNameBid
        let whois = Whois
              { whoisOwner = msgBuyNameBuyer
              , whoisValue = msgBuyNameValue
              , whoisPrice = msgBuyNameBid
              }
        putWhois msgBuyNameName whois
        emit NameClaimed
          { nameClaimedOwner = msgBuyNameBuyer
          , nameClaimedName = msgBuyNameName
          , nameClaimedValue = msgBuyNameValue
          , nameClaimedBid = msgBuyNameBid
          }

      buyClaimedName
        :: HasNameserviceEff r
        => HasTokenEff r
        => Member (Output Event) r
        => MsgBuyName
        -> Whois
        -> Sem r ()
      buyClaimedName MsgBuyName{..} currentWhois =
        let Whois{ whoisPrice = forsalePrice, whoisOwner = previousOwner } = currentWhois
        in if msgBuyNameBid > forsalePrice
             then do
               transfer msgBuyNameBuyer msgBuyNameBid previousOwner
               putWhois msgBuyNameName currentWhois {whoisOwner = msgBuyNameBuyer}
               emit NameClaimed
                 { nameClaimedOwner = msgBuyNameBuyer
                 , nameClaimedName = msgBuyNameName
                 , nameClaimedValue = msgBuyNameValue
                 , nameClaimedBid = msgBuyNameBid
                 }
             else throw (InsufficientBid "Bid must exceed the price.")
