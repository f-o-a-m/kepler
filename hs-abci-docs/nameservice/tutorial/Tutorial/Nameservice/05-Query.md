---
title: Nameservice - Query
---

# Query

## Tutorial.Nameservice.Query

~~~ haskell
module Tutorial.Nameservice.Query where

import Nameservice.Modules.Nameservice.Types (Whois)
import Nameservice.Modules.Nameservice.Store (Name, whoisMap)
import Polysemy (Members)
import Tendermint.SDK.BaseApp (QueryEffs, StoreLeaf, RouteQ, storeQueryHandler)
import qualified Tendermint.SDK.BaseApp.Store.Map as M
import Servant.API ((:>))
~~~

The way to query application state is via the `query` message which uses a `url` like format. The SDK tries to abstract as much of this away as possible. For example, if you want to only serve state that you have registered with the store via the `IsKey` class, then things are very easy. If you need to make joins to serve requests, we support this as well and it's not hard, but we will skip this for now.

In the case we just want to serve data we have registered with the `IsKey` class, we simply need to declare some types:

```haskell

type Api = "whois" :> StoreLeaf (M.Map Name Whois)

```

`Api` is the list of valid urls for this module. In this case, when unpacked it will create a single endpoint `<URL_ROOT>/whois` that expects a value of type `Name` as the `data` field in the ABCI `query` object. Technically speaking it is a `Name` prefixed by some other storage related prefixes dictated by the module, but we will hold off on this for now. If you use the automatically generated client libraries, you don't need to worry about this.

To serve all the data, we can use the `storeQueryHandler` function by supplying the appropriate `store` we want to serve. 

~~~ haskell
querier
  :: Members QueryEffs r
  => RouteQ Api r
querier = storeQueryHandler whoisMap
~~~

Here `RouteQ` is a type family that can build a server from the `Api` type to handle incoming requests. It is similar to how `servant` works, and is largely vendored from that codebase.

Note that more advanced queries are possible other than just serving what is in storage. For example you might want to use joins to fulfill requests or use query parameters in the url. These are all possible, but we won't go into details here as they are not used in the app.
