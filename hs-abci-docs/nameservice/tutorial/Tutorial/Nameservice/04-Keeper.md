---
title: Nameservice - Keeper
---

# Keeper

## Definition

"Keeper" is a word taken from the cosmos-sdk, it's basically the interface that the module exposes to the other modules in the application. For example, in the Nameservice app, the Nameservice keeper exposes functions to `buy`/`sell`/`delete` entries in the mapping. Likewise, the Nameservice keeper depends on the keeper from the `bank` module in order to transfer tokens when executing those methods. A keeper might also indicate what kinds of exceptions are able to be caught and thrown from the module. For example, calling `transfer` while buying a `Name` might throw an `InsufficientFunds` exception, which the Namerservice module can chose whether to catch or not.

## Tutorial.Nameservice.Keeper

In this section, we will make use of the `Store` types defined in `Nameservice.Modules.Nameservice.Store`. For an overview on how this is setup, see the `Storage` chapter in the `Foundations` section of the tutorial.

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}
module Tutorial.Nameservice.Keeper where

import Polysemy (Sem, Member, Members, makeSem)
import Polysemy.Error (Error, throw)
import Nameservice.Modules.Nameservice.Messages
import Nameservice.Modules.Nameservice.Store (Name(..), whoisMap)
import Nameservice.Modules.Nameservice.Types (Whois(..), NameDeleted(..), NameserviceError(..))
import qualified Tendermint.SDK.BaseApp as BA
import qualified Tendermint.SDK.BaseApp.Store.Map as M
import Tendermint.SDK.Modules.Bank (BankEffs, Coin(..), CoinId, mint)


nameserviceCoinId :: CoinId
nameserviceCoinId = "nameservice"
~~~

Generally a keeper is defined by a set of effects that the module introduces and depends on. In the case of Nameservice, we introduce the custom `Nameservice` effect:


~~~ haskell
type NameserviceEffs = '[NameserviceKeeper, Error NameserviceError]

data NameserviceKeeper m a where
  BuyName :: BuyNameMsg -> NameserviceKeeper m ()
  DeleteName :: DeleteNameMsg -> NameserviceKeeper m ()
  SetName :: SetNameMsg -> NameserviceKeeper m ()
  GetWhois :: Name -> NameserviceKeeper m (Maybe Whois)

makeSem ''NameserviceKeeper
~~~

where `makeSem` is from polysemy, it uses template Haskell to create the helper functions `buyName`, `deleteName`, `setName`, `getWhois`:

~~~ haskell ignore
buyName :: BuyNameMsg -> NameserviceKeeper m ()
deleteName :: DeleteNameMsg -> NameserviceKeeper m ()
setName :: SetNameMsg -> NameserviceKeeper m ()
getWhois :: Name -> NameserviceKeeper m (Maybe Whois)
~~~

### Evaluating Module Effects

Like we said before, all transactions must ultimately compile to the set of effects belonging to `TxEffs` and `BaseEffs`. In particular this means that we must interpret `NameserviceEffs` into more basic effects. To do this we follow the general pattern of first interpreting `NameserviceKeeper` effects, then finally interpreting `Error NameserviceError` in terms of `Error AppError`. Let's focus on the `DeleteName` summand of `NameserviceKeeper`. We can write an interpreting function as follows:

~~~ haskell
deleteNameF
  :: Members BA.TxEffs r
  => Members BA.BaseEffs r
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
          BA.emit event
          BA.logEvent event
~~~

The control flow should be pretty clear:
1. Check that the name is actually registered, if not throw an error.
2. Check that the name is registered to the person trying to delete it, if not throw an error.
3. Refund the tokens locked in the name to the owner.
4. Delete the entry from the database.
5. Emit an event that the name has been deleted and log this event.

Taking a look at the class constraints, we see

~~~ haskell ignore
  ( Members BaseApp.TxEffs r
  , Members BaseApp.BaseEffs r
  , Members BankEffs r
  , Member (Error NameserviceError) r
  )
~~~

- The `TxEffs` effect is required because the function manipulates the `whoisMap` and emits an `Event`.
- The `BaseEffs` effect is required because the function has logging.
- The `Error NameserviceError` effect is required because the function may throw an error.
- The `BankEffs` effect is required because the function will mint coins.


Using this helper function and others, we can write our module's `eval` function by interpreting the `NameserviceEffs` in two steps:

~~~ haskell ignore
eval
  :: Members BA.TxEffs r
  => Members BankEffs r
  => Members BA.BaseEffs r
  => forall a. Sem (NameserviceKeeper ': Error NameserviceError ': r) a
  -> Sem r a
eval = mapError BaseApp.makeAppError . evalNameservice
  where
    evalNameservice
      :: Members BA.TxEffs r
      => Members BA.BaseEffs r
      => Members BankEffs r
      => Member (Error NameserviceError) r
      => Sem (NameserviceKeeper ': r) a -> Sem r a
    evalNameservice =
      interpret (\case
          ...
          DeleteName msg -> deleteNameF msg
          ...
        )

~~~
