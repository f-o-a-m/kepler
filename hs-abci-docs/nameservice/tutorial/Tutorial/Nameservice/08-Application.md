---
title: Nameservice - Application
---

# Application

## From Modules to App

The `App` type in `Network.ABCI.Server` is defined as

~~~ haskell ignore
newtype App m = App
  { unApp :: forall (t :: MessageType). Request t -> m (Response t) }
~~~

and ultimately our configuration of modules must be converted to this format. This is probably the most important part of the SDK, to provide the bridge between the list of modules - a heterogeneous list of type `ModuleList` - and the actual application. The type that provides the input for this bridge is `HandlersContext`:

~~~ haskell ignore
data HandlersContext alg ms core = HandlersContext
  { signatureAlgP :: Proxy alg
  , modules       :: M.ModuleList ms (Effs ms core)
  , beginBlockers :: [Req.BeginBlock ->
    Sem (BlockEffs BA.:& BA.BaseAppEffs core) Resp.BeginBlock]
  , endBlockers   :: [Req.EndBlock ->
    Sem (BlockEffs BA.:& BA.BaseAppEffs core) Resp.EndBlock]
  , anteHandler   :: BA.AnteHandler (Effs ms core)
  , compileToCore :: forall a. Sem (BA.BaseAppEffs core) a -> Sem core a
  }
~~~

where
- `alg` is the signature schema you would like to use for authentication (e.g. Secp256k1)
- `ms` is the type level list of modules
- `r` is the global effects list for the application
- `core` is the set of core effects that are used to interpret `BaseApp` to `IO`.
- `Effs` is a type family that gathers the effect dependencies for `ms` in the appropriate order.

We should say a few words on this `compileToCore` field. The application developer has to, at the end of the day, specify how the entire effects system for the application will be interpreted to `IO`. Luckily most of these decisions are abstracted away, but the one that remains is dealing with `BaseApp core`. The sdk provides two default methods for two different types of `core`:


~~~ haskell ignore
defaultCompileToCore
  :: forall a.
     Sem (BaseApp CoreEffs) a
  -> Sem CoreEffs a

defaultCompileToPureCore
  :: forall a.
     Sem (BaseApp PureCoreEffs) a
  -> Sem PureCoreEffs a
~~~


The difference is that `defaultCompileToCore` uses the IAVL store external database and also allows for metrics, where `defaultCompileToPureCore` uses an in-memory db and treats all metrics operations as a no-op.

## Tutorial.Nameservice.Application

~~~ haskell
module Tutorial.Nameservice.Application where

import Data.Proxy
import Nameservice.Modules.Nameservice (Nameservice, nameserviceModule)
import Network.ABCI.Server.App (App)
import Polysemy (Sem)
import Tendermint.SDK.Modules.Auth (Auth, authModule)
import Tendermint.SDK.Application (ModuleList(..), HandlersContext(..), baseAppAnteHandler, makeApp, createIOApp)
import Tendermint.SDK.BaseApp (CoreEffs, Context, defaultCompileToCore, runCoreEffs)
import Tendermint.SDK.Crypto (Secp256k1)
import Tendermint.SDK.Modules.Bank (Bank, bankModule)
~~~

At this point we need to simply list the modules we want to use in our application. We only require that if a module is declared as a dependency by another (via the `deps` type variable in the `Module` type), then that dependency should be inserted below that module. For example, since `Nameservice` depends on `Bank`, we must list `Bank` after `Nameservice`. Similarly since `Bank` depends on `Auth`, we must list `Auth` after `Bank`:


~~~ haskell
type NameserviceModules =
   '[ Nameservice
    , Bank
    , Auth
    ]
~~~

We're now ready to define the `HandlersContext` for our application:

~~~ haskell
handlersContext :: HandlersContext Secp256k1 NameserviceModules CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , beginBlockers = []
  , endBlockers = []
  , compileToCore  = defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
  nameserviceModules =
       nameserviceModule
    :+ bankModule
    :+ authModule
    :+ NilModules
~~~

Finally we're able to define our application that runs in the `CoreEffs` context defined in the SDK:


~~~ haskell
app :: App (Sem CoreEffs)
app = makeApp handlersContext
~~~

Since the ABCI server requires you to pass a value of type `App IO`, we have one more transformation to perform to get the `Sem CoreEffs` in our app. We can simple use the `createIOApp` function:

~~~ haskell
makeIOApp :: Context -> App IO
makeIOApp ctx = createIOApp (runCoreEffs ctx) app
~~~
