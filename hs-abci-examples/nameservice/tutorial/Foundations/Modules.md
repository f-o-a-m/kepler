# Modules

## Definition

`Module` has a very specific meaning in the context of this SDK. A `Module` is something between a library and a small state machine. It is built on top of the `BaseApp` abstraction in the sense that all `Module`s must be explicitly interpeted in terms of `BaseApp` in order to compile the application. The full type definition is

```haskell
data Module (name :: Symbol) msg (api :: *) (s :: EffectRow) (r :: EffectRow) = Module
  { moduleTxDeliverer :: RoutedTx msg -> Sem r ()
  , moduleTxChecker   :: RoutedTx msg -> Sem r ()
  , moduleQueryServer :: RouteT api (Sem r)
  , moduleEval :: forall deps. Members BaseAppEffs deps => forall a. Sem (s :& deps) a -> Sem deps a
  }
```

where

- `name` is the name of the module, e.g. `"bank"`.
- `msg` is the type of the incoming messages the module must handle.
- `api` is the query api for querying state in the url format (more on this later).  
- `s` is the set of effects introduced by this module.
- `r` is the global set of effects that this module will run in when part of a larger application (more on this later).

The fields for the `Module` data type are:

  - `moduleTxDeliverer`: This specifies how the module processes transactions to update the application state during `deliverTx` messages. 
  - `moduleTxChecker`: This is used during `checkTx` messages to check if a transaction in the mempool is a valid transaction.
  - `moduleQueryServer`: This is responsible for handling queries for application state from the `query` message.
  - `moduleEval`: This is the natural transformation that specifies how to interpet the `Module` in terms of `BaseApp`.

Note that in the event that a `Module` is _abstract_, meaning it doesn't have any messages to respond to, then we have `msg ~ Void`.

## Composition

`Module`s are meant to be composed to create larger applications. We will see examples of this with the `Nameservice` application. The way to do this is easy, the `Modules` data type allows you to simply combine them in a heterogeneous list:

```haskell
data Modules (ms :: [*]) r where
    NilModules :: Modules '[] r
    (:+) :: Module name msg api s r -> Modules ms r -> Modules (Module name msg api s r  ': ms) r
```

when you are ready to create your application, you simply specify a value of type `Modules` and some other configuration data, and the SDK will create an `App` for you.