# hs-abci-server

The `hs-abci-server` package defines the types and methods for serving an ACBI application. See 
the [example application](https://github.com/f-o-a-m/hs-abci/tree/master/hs-abci-example) as a guide for how you can use this package.

## Application types
The applicaction type `App m` is defined as a newtype

```haskell
newtype App m = App m
  { unApp :: forall (t :: MessageType). Request t -> m (Response t) } 
```

where `m` is the context in which you define your handlers. Here `MessageType` is being used as a promoted kind to ensure well
typed handlers, i.e. you are forced to write handlers like

```haskell
handleBeginBlock
  :: Request 'MTBeginBlock
  -> m (Response 'MTBeginBlock)
handleBeginBlock req = ...
```

The application is served over TCP using `Data.Conduit.Network`, which means that ultimately your application must be run
in `IO`. You can use the `transformApp` to compile your application to `IO`

```haskell
transformApp
  :: (forall (t :: MessageType). m (Response t) -> IO (Response t)
  -> App m
  -> App IO
```

## Middleware
In the style of WAI, we supply a middleware type as

```haskell
type Middleware m = App m -> App m
```

This is useful for hooking in things like metrics, loggers, etc. You can find some out-of-the-box middleware solutions in
the [hs-abci-extra](https://github.com/f-o-a-m/hs-abci/tree/master/hs-abci-extra) package.
