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
data HandlersContext alg ms r core = HandlersContext
  { signatureAlgP :: Proxy alg
  , modules       :: M.ModuleList ms r
  , anteHandler   :: BA.AnteHandler r
  , compileToCore :: forall a. Sem (BA.BaseApp core) a -> Sem core a
  }
~~~

where
- `alg` is the signature schema you would like to use for authentication (e.g. Secp256k1)
- `ms` is the type level list of modules
- `r` is the global effects list for the application
- `core` is the set of core effects that are used to interpet `BaseApp` to `IO`.

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
import Nameservice.Modules.Nameservice (nameserviceModule, NameserviceM, NameserviceEffs)
import Nameservice.Modules.Token (tokenModule, TokenM, TokenEffs)
import Network.ABCI.Server.App (App)
import Polysemy (Sem)
import Tendermint.SDK.Modules.Auth (authModule, AuthEffs, AuthM)
import Tendermint.SDK.Application (ModuleList(..), HandlersContext(..), baseAppAnteHandler, makeApp, createIOApp)
import Tendermint.SDK.BaseApp (BaseApp, CoreEffs, Context, TxEffs, (:&), defaultCompileToCore, runCoreEffs)
import Tendermint.SDK.Crypto (Secp256k1)
~~~

This is the part of the application where the effects list must be given a monomorphic type. The only requirement is that you list the effects in the same order that the corresponding modules appear in the `NameserviceModules` list:


~~~ haskell
type EffR =
   NameserviceEffs :&
   TokenEffs :&
   AuthEffs :&
   TxEffs :&
   BaseApp CoreEffs

type NameserviceModules =
   '[ NameserviceM EffR
    , TokenM EffR
    , AuthM EffR
    ]
~~~

Also notice that `TxEffs :& BaseApp CoreEffs` appears at the end of the effects list, but doesn't strictly corrospond to a module. This needs to be here as ultimately all transactions and queries are intereted to `TxEffs :& BaseApp CoreEffs` before being run.

We're now ready to define the `HandlersContext` for our application:

~~~ haskell
handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToCore  = defaultCompileToCore
  , anteHandler = baseAppAnteHandler
  }
  where
  nameserviceModules :: ModuleList NameserviceModules EffR
  nameserviceModules =
       nameserviceModule
    :+ tokenModule
    :+ authModule
    :+ NilModules
~~~

Finally we're able to define our application that runs in the `CoreEffs` context defined in the SDK:


~~~ haskell
app :: App (Sem CoreEffs)
app = makeApp handlersContext 
~~~

Since the ABCI server requires you to pass a value of type `App IO`, we have one more transformation to perform to get replace the `Sem CoreEffs` in our app. We can simple use the `createIOApp` function:

~~~ haskell
makeIOApp :: Context -> App IO
makeIOApp ctx = createIOApp (runCoreEffs ctx) app
~~~
