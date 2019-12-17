# Nameservice.Keeper

## Keeper

"Keeper" is a word taken from the cosmos-sdk, it's basically the interface that the module exposes to the other modules in the application. For example, in the Nameservice app, the Nameservice keeper exposes functions to `buy`/`sell`/`delete` entries in the mapping. Likewise, the Nameservice keeper depends on the keeper from the `bank` module in order to transfer tokens when executing those methods. A keeper might also indicate what kinds of exceptions are able to be caught and thrown from the module. For example, calling `transfer` while buying a `Name` might throw an `InsufficientFunds` exception, which the Namerservice module can chose to catch or not.

## Nameservice.Keeper

Generally a keeper is defined by a set of effects that the module introduces and depends on. In the case of Nameservice, we introduce the custom `Nameservice` effect:


```haskell
data Nameservice m a where
  PutWhois :: Name -> Whois -> Nameservice m ()
  GetWhois :: Name -> Nameservice m (Maybe Whois)
  DeleteWhois :: Name -> Nameservice m ()

makeSem ''Nameservice
```

where `makeSem` is from polysemy, it uses template Haskell to create the helper functions putWhoIs, getWhois, deleteWhois:

```haskell
putWhois :: forall r. Member Nameservice r => Name -> Whois -> Sem r ()
getWhois :: forall r. Member Nameservice r => Name -> Sem r (Maybe Whois)
deleteWhois :: forall r. Member Nameservice r => Name -> Sem r ()
```

We can then write the top level function for example for deleting a name:

```haskell
deleteName
  :: Members [Token, Output BaseApp.Event] r
  => Members [Nameservice, Error NameserviceError] r
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
          BaseApp.emit NameDeleted
            { nameDeletedName = deleteNameName
            }
```

The control flow should be pretty clear:
1. Check that the name is actually registered, if not throw an error.
2. Check that the name is registered to the person trying to delete it, if not throw an error.
3. Refund the tokens locked in the name to the owner.
4. Delete the entry from the database.
5. Emit an event that the name has been deleted.

Taking a look at the class constraints, we see

```haskell
(Members NameserviceEffs, Members [Token, Output Event] r)
```

- The `Nameservice` effect is required because the function may manipulate the modules database with `deleteName`.
- The `Error NameserviceError` effect is required because the function may throw an error.
- The `Token` effect is required because the function will mint coins.
- The `Output Event` effect is required because the function may emit a `NameDeleted` event.

### Evaluating Module Effects

Like we said before, all modules must ultimately compile to the set of effects belonging to `BaseApp`. For effects interpreted to `RawStore`, this means that you will need to define something called a `StoreKey`. 


A `StoreKey` is effectively a namespacing inside the database, and is unique for a given module. In theory it could be anything, but the natural definition in the case of `Nameservice` is would be something like

```haskell
storeKey :: StoreKey NameserviceModule
storeKey = StoreKey . cs . symbolVal $ (Proxy :: Proxy NameserviceModuleName)
```
With this `storeKey` it is possible to write the `eval` function to resolve the effects defined in Nameservice, namely the `Nameservice` effect and `Error NameserviceError`:

```haskell
eval
  :: Members [RawStore, Error AppError] r
  => forall a. Sem (Nameservice ': Error NameserviceError ': r) a
  -> Sem r a
eval = mapError makeAppError . evalNameservice
  where
    evalNameservice
      :: Members [RawStore, Error AppError] r
      => Sem (Nameservice ': r) a -> Sem r a
    evalNameservice =
      interpret (\case
          GetWhois name -> get storeKey name
          PutWhois name whois -> put storeKey name whois
          DeleteWhois name -> delete storeKey name
        )
```