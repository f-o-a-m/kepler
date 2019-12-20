# Keeper

## Definition

"Keeper" is a word taken from the cosmos-sdk, it's basically the interface that the module exposes to the other modules in the application. For example, in the Nameservice app, the Nameservice keeper exposes functions to `buy`/`sell`/`delete` entries in the mapping. Likewise, the Nameservice keeper depends on the keeper from the `bank` module in order to transfer tokens when executing those methods. A keeper might also indicate what kinds of exceptions are able to be caught and thrown from the module. For example, calling `transfer` while buying a `Name` might throw an `InsufficientFunds` exception, which the Namerservice module can chose whether to catch or not.

## Tutorial.Nameservice.Keeper

~~~ haskell
{-# LANGUAGE TemplateHaskell #-}
module Tutorial.Nameservice.Keeper where

import Data.Proxy
import Data.String.Conversions (cs)
import GHC.TypeLits (symbolVal)
import Polysemy (Sem, Members, makeSem, interpret)
import Polysemy.Error (Error, throw, mapError)
import Polysemy.Output (Output)
import Nameservice.Modules.Nameservice.Messages (DeleteName(..))
import Nameservice.Modules.Nameservice.Types (Whois(..), Name, NameDeleted(..), NameserviceModuleName, NameserviceError(..))
import Nameservice.Modules.Token (Token, mint)
import qualified Tendermint.SDK.BaseApp as BA
~~~

Generally a keeper is defined by a set of effects that the module introduces and depends on. In the case of Nameservice, we introduce the custom `Nameservice` effect:


~~~ haskell
data NameserviceKeeper m a where
  PutWhois :: Name -> Whois -> NameserviceKeeper m ()
  GetWhois :: Name -> NameserviceKeeper m (Maybe Whois)
  DeleteWhois :: Name -> NameserviceKeeper m ()

makeSem ''NameserviceKeeper

type NameserviceEffs = '[NameserviceKeeper, Error NameserviceError]
~~~

where `makeSem` is from polysemy, it uses template Haskell to create the helper functions `putWhoIs`, `getWhois`, `deleteWhois`:

~~~ haskell ignore
putWhois :: forall r. Member NameserviceKeeper r => Name -> Whois -> Sem r ()
getWhois :: forall r. Member NameserviceKeeper r => Name -> Sem r (Maybe Whois)
deleteWhois :: forall r. Member NameserviceKeeper r => Name -> Sem r ()
~~~

We can then write the top level function for example for deleting a name:

~~~ haskell
deleteName
  :: Members [Token, Output BA.Event] r
  => Members [NameserviceKeeper, Error NameserviceError] r
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
          BA.emit NameDeleted
            { nameDeletedName = deleteNameName
            }
~~~

The control flow should be pretty clear:
1. Check that the name is actually registered, if not throw an error.
2. Check that the name is registered to the person trying to delete it, if not throw an error.
3. Refund the tokens locked in the name to the owner.
4. Delete the entry from the database.
5. Emit an event that the name has been deleted.

Taking a look at the class constraints, we see

~~~ haskell ignore
(Members NameserviceEffs, Members [Token, Output Event] r)
~~~

- The `NameserviceKeeper` effect is required because the function may manipulate the modules database with `deleteName`.
- The `Error NameserviceError` effect is required because the function may throw an error.
- The `Token` effect is required because the function will mint coins.
- The `Output Event` effect is required because the function may emit a `NameDeleted` event.

### Evaluating Module Effects

Like we said before, all modules must ultimately compile to the set of effects belonging to `BaseApp`. For effects interpreted to `RawStore`, this means that you will need to define something called a `StoreKey`.


A `StoreKey` is effectively a namespacing inside the database, and is unique for a given module. In theory it could be any `ByteString`, but the natural definition in the case of Nameservice is would be something like

~~~ haskell
storeKey :: BA.StoreKey NameserviceModuleName
storeKey = BA.StoreKey . cs . symbolVal $ (Proxy @NameserviceModuleName)
~~~

With this `storeKey` it is possible to write the `eval` function to resolve the effects defined in Nameservice, namely the `NameserviceKeeper` effect and `Error NameserviceError`:

~~~ haskell
eval
  :: Members [BA.RawStore, Error BA.AppError] r
  => forall a. Sem (NameserviceKeeper ': Error NameserviceError ': r) a
  -> Sem r a
eval = mapError BA.makeAppError . evalNameservice
  where
    evalNameservice
      :: Members [BA.RawStore, Error BA.AppError] r
      => Sem (NameserviceKeeper ': r) a -> Sem r a
    evalNameservice =
      interpret (\case
          GetWhois name -> BA.get storeKey name
          PutWhois name whois -> BA.put storeKey name whois
          DeleteWhois name -> BA.delete storeKey name
        )
~~~

[Next: Query](Query.md)
