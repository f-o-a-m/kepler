---
title: Foundations - Effects
---

# Effects Lists

There are several distinguished effects lists you should be familiar with that come up at different parts of the sdk or application development. They are often combined or stacked using the `:&` operator, which is simply concatenation of type level lists.


## BaseEffs

`BaseEffs` is a set of effects that the SDK operates with and are freely available
for an application developer to make use of in any part of their application code. They
are defined as:

~~~ haskell ignore
type BaseEffs =
  [ Metrics
  , Logger
  , Resource
  , Error AppError
  ]
~~~

These effects are:

1. `Metrics` - creates and manages custom counters and timers.
2. `Logger` - allows for structured logging with log levels.
3. `Resource` - allows for bracketing and resource management in the presence of exceptions.
4. `Error AppError` - allows for errors of type `AppError` to be thrown or caught.

The SDK does not make any assumptions about how `BaseEffs` will be interpreted at runtime, it only assumes that the developer might want use one of the provided core effects systems to interpret them. For example, the standard `CoreEffs` uses a prometheus metrics server to interpret the `Metrics` effect while `PureCoreEffs` just ignores the effect entirely.

## TxEffs

`TxEffs` are the effects used to interpret transactions and are defined as

~~~ haskell ignore
type TxEffs =
  [ Output Event
  , GasMeter
  , WriteStore
  , ReadStore
  , Error AppError
  ]
~~~

where

1. `Output Event` - allows for emitting events during transaction execution.
2. `GasMeter` - allows for gas costs to be levied at any place during transaction code.
3. `WriteStore` - allows for put/delete operations on the database.
4. `ReadStore` - allows for get/prove operations on the database.
5. `Error AppError` - allows for throwing and catching errors raised during transactions.

`TxEffs` effects are available any time during transactions and are interpreted at the time of transaction routing. It's worth noting that the interpreters take care of finalizing writes to the database when it's appropriate (i.e. during a `deliverTx` message) and not otherwise (e.g. during a `checkTx` message).

## QueryEffs

`QueryEffs` are used to interpret queries and are defined as

~~~ haskell ignore
type QueryEffs =
  [ ReadStore
  , Error AppError
  ]
~~~

where

1. `ReadStore` allows for get/prove operations on the database.
2. `Error AppError` allows for throwing/catching errors when performing database queries.

`QueryEffs` are available any time you are writing handlers for a module's query api. The SDK manages a separate connection for reading from committed (i.e. via blocks) state when `QueryEffs` are present.

## StoreEffs

`StoreEffs` describe the possible interactions with an abstract merkelized key-value database:

~~~ haskell ignore
type StoreEffs =
  [ Tagged 'Consensus ReadStore
  , Tagged 'QueryAndMempool ReadStore
  , Tagged 'Consensus WriteStore
  , Transaction
  , CommitBlock
  ]
~~~

They are used to interpret `TxEffects` depending on what context you're in, e.g. while executing a `deliverTx` versus `checkTx` message, or a `query`. Some effects are tagged with a promoted value of type `Scope`, i.e. `'Consensus` and `'QueryAndMempool`. This is because your application will keep multiple connections to the database that are used in different situations. For example, since writing to the state at a previous blocktime is impossible, we disallow writing to the database in such instances.

# Effects Type Synonyms

There are a few effects lists that appear so frequently at different points in the SDK that they deserve synonyms:

## Effs

`Effs` is technically a type family coming from the class `Tendermint.SDK.Application.Module.Eval`. It is used to enumerate all the effects that would be needed in order to run an application defined by a `ModuleList`.

## BaseAppEffs

There is a type alias in the SDK called `BaseAppEffs` with the definition

~~~ haskell ignore
type BaseApp core = BaseEffs :& StoreEffs :& core
~~~

It sits at the bottom of any application's effects list and ultimately everything is interpreted through these effects before being run, e.g. `TxEffs` and `QueryEffs`. This effects list is pretty much the only place where the application developer needs to decide how the interpretation should be done. There are two options in the SDK, using `PureCoreEffs` or `CoreEffs` depending on whether you want to rely on an external or in-memory database.
