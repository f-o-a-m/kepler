# Query

## Tutorial.Nameservice.Query

~~~ haskell
module Tutorial.Nameservice.Query where

import Data.Proxy
import Nameservice.Modules.Nameservice.Keeper (storeKey)
import Nameservice.Modules.Nameservice.Types (Whois, Name)
import Polysemy (Sem, Members)
import Polysemy.Error (Error)
import Tendermint.SDK.BaseApp (RawStore, AppError, RouteT, QueryApi, storeQueryHandlers)
~~~

The way to query application state is via the `query` message which uses a `url` like format. The SDK tries to abstract as much of this away as possible. For example, if you want to only serve state that you have registered with the store via the `IsKey` class, then things are very easy. If you need to make joins to serve requests, we support this as well and it's not hard, but we will skip this for now.

In the case we just want to serve data we have registered with the `IsKey` class, we simply need to declare some types

```haskell
type NameserviceContents = '[(Name, Whois)]

type Api = QueryApi NameserviceContents
```

- `NameserviceContents` is simply a type level list of the key value pairs you wish to serve. In this case there is only `Name -> Whois`
- `Api` is the list of leaves of valid url's for this module. When the type family `QueryApi` is applied, it will construct the leaves from the key value pairs based on the `IsKey` class. In this case you end up with only `"/whois"` endpoint, which accepts the `Name` in the `data` field of the `query` message encoded via the `HasCodec` class.

To serve all the data registered with the `IsKey` class, we can use the `storeQueryHandlers` function, supplying a proxy for the store contents, the `storeKey` and a proxy for the effects used in serving requests. In this case because we are serving only types registered with the store, we will need to assume the `RawStore` and `Error AppError` effects.

~~~ haskell
server
  :: Members [RawStore, Error AppError] r
  => RouteT Api (Sem r)
server =
  storeQueryHandlers (Proxy @NameserviceContents) storeKey (Proxy :: Proxy (Sem r))
~~~

Here `RouteT` is a type family that can build a server from the `Api` type to handle incoming requests. It is similar to how `servant` works, and is largely copy-pasted from that codebase.

[Next: Module](Module.md)
