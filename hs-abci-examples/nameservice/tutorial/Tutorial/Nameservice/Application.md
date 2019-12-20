# Application

## From Modules to App

The `App` type in `Network.ABCI.Server` is defined as 

~~~ haskell ignore
newtype App m = App
  { unApp :: forall (t :: MessageType). Request t -> m (Response t) }
~~~

and ultimately our configuration of modules must be converted to this format. This is probably the most important part of the SDK, to provide the bridge between the list of modules - a heterogeneous list of type `Modules` - and the actual application. The type that provides the input for this bridge is `HandlersContext`:

~~~ haskell ignore
data HandlersContext alg ms r core = HandlersContext
  { signatureAlgP :: Proxy alg
  , modules       :: M.Modules ms r
  , compileToCore :: forall a. ScopedEff core a -> Sem core a
  }
~~~

where
- `alg` is the signature schema you would like to use for authentication (e.g. Secp256k1)
- `ms` is the type level list of modules
- `r` is the global effects list for the application
- `core` is the set of core effects that are used to interpet `BaseApp` to `IO`.

We should say a few words on this `compileToCore` field. The application developer has access to any effects in `BaseApp`, 
but `BaseApp` itself still needs to be interpreted in order to run the application. In other words, `BaseApp` is still just a 
list of free effects. The set of effects capable of interpreting `BaseApp` is called `core`, and while the developer is free to provide any `core` they want, we have a standard set of them in the SDK - e.g. in memory, production, etc. 

The `ScopedEff` type is more complicated and not relevant to the discussion of application development. Long story short, tendermint core requests three connections to the application's state - `Consensus`, `Mempool` and `Query`. The `ScopedEff` type is used to abstract this concern away from the developer, and as long as you are using one of the `core` effects provided in the SDK you don't need to worry about it.

## Tutorial.Nameservice.Application

~~~ haskell
module Tutorial.Nameservice.Application where

import Data.Proxy
import Nameservice.Modules.Nameservice (nameserviceModule, NameserviceM, NameserviceEffs)
import Nameservice.Modules.Token (tokenModule, TokenM, TokenEffs)
import Network.ABCI.Server.App (App)
import Polysemy (Sem)
import Tendermint.SDK.Modules.Auth (authModule, AuthEffs, AuthM)
import Tendermint.SDK.Application (Modules(..), HandlersContext(..), makeApp)
import Tendermint.SDK.BaseApp (BaseApp, CoreEffs, (:&), compileScopedEff)
import Tendermint.SDK.Crypto (Secp256k1)
~~~

This is the part of the application where the effects list must be given a monomorphic type. There is also a requirement
that the `Modules` type for the application be given the same _order_ as the effects introducted. This ordering problem is due
to the fact that type level lists are used to represent the effects in `polysemy`, and the order matters there. Still, it's only a small annoyance.


~~~ haskell
type EffR =
   NameserviceEffs :&
   TokenEffs :&
   AuthEffs :&
   BaseApp CoreEffs

type NameserviceModules =
   '[ NameserviceM EffR
    , TokenM EffR
    , AuthM EffR
    ]
~~~

Notice that we've specified `EffR` as the effects list for each of the modules to run in, which trivially satisfies the constraints on each module at the definition site, since it is simply the union of all effects.

We're now ready to define the `HandlersContext` for our application:

~~~ haskell
handlersContext :: HandlersContext Secp256k1 NameserviceModules EffR CoreEffs
handlersContext = HandlersContext
  { signatureAlgP = Proxy @Secp256k1
  , modules = nameserviceModules
  , compileToCore  = compileScopedEff
  }
  where
  nameserviceModules :: Modules NameserviceModules EffR
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
