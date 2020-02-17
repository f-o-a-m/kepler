---
title: Foundations - Module
---

# Modules

## Definition

A `Module` has a very specific meaning in the context of this SDK. A `Module` is something between a library and a small state machine. Here is the type:

~~~ haskell ignore
data Module (name :: Symbol) (check :: *) (deliver :: *) (query :: *) (es :: EffectRow) (r :: EffectRow)  = Module
  { moduleTxChecker :: T.RouteTx check r
  , moduleTxDeliverer :: T.RouteTx deliver r
  , moduleQuerier :: Q.RouteQ query r
  , moduleEval :: forall deps. Members T.TxEffs deps => forall a. Sem (es :& deps) a -> Sem deps a
  }
~~~

where the type parameters

- `name` is the name of the module, e.g. `"bank"`.
- `check` is the transaction router api type for `checkTx` messages.
- `deliver` is the transaction router api type for `checkTx` messages.
- `query` is the query router api type for `query` messages
- `es` is the set of effects introduced by this module.
- `r` is the global set of effects that this module will run in when part of a larger application (more on this later).

Below that line we see the fields for the `Module` data type, where

  - `moduleTxDeliverer` specifies how the module processes transactions in order to update the application state during `deliverTx` messages. 
  - `moduleTxChecker` is used during `checkTx` messages to check if a transaction in the mempool is a valid transaction.
  - `moduleQuerier` is responsible for handling queries for application state from the `query` message.
  - `moduleEval` is the natural transformation that specifies how to interpet the `Module` in terms of `BaseApp`.

If you have ever used the `servant` library for specifying rest apis, then the type families `T.RouteTx` and `Q.RouteQ` may look familiar to you, they play a similar role as `ServerT`.

Note that in the event that a `Module` is _abstract_, meaning it doesn't have any messages to respond to, then we have `msg ~ Void`.

## Composition

`Module`s are meant to be composed to create larger applications. We will see examples of this with the `Nameservice` application. The way to do this is easy, as the `ModuleList` data type allows you to simply combine them in a heterogeneous list:

~~~ haskell ignore
data ModuleList (ms :: [*]) r where
    NilModules :: Modules '[] r
    (:+) :: Module name check deliver query es r 
         -> Modules ms r 
         -> Modules (Module name check deliver query es r  ': ms) r
~~~

When you are ready to create your application, you simply specify a value of type `ModuleList` and some other configuration data, and the SDK will create an `App` for you.
