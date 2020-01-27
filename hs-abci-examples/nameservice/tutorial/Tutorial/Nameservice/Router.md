
# Router

## Tutorial.Nameservice.Router

The Router is where you specifiy the handlers for the messages that the module accepts. The router is typed in a [servant](https://hackage.haskell.org/package/servant) style, using combinators and primitives to declare a very precise type for the router.

~~~ haskell
module Tutorial.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (NameserviceEffs,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (BuyName, DeleteName,
                                                           SetName)
import           Nameservice.Modules.Token                (TokenEffs)
import           Polysemy                                 (Members, Sem)
import           Servant.API                              ((:<|>) (..))
import           Tendermint.SDK.BaseApp                   ((:~>), BaseAppEffs,
                                                           Return,
                                                           RouteContext (..),
                                                           RouteTx,
                                                           RoutingTx (..),
                                                           TxEffs, TypedMessage,
                                                           incCount, withTimer)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (Tx (..))

~~~

## Typing the Router

First we declare the type for our router

~~~ haskell
type MessageApi =
       TypedMessage BuyName :~> Return ()
  :<|> TypedMessage SetName :~> Return ()
  :<|> TypedMessage DeleteName :~> Return ()
~~~

Lets break it down:

- `(:<|>)` is the operator which denotes alternative, so our router is composed of 3 handlers in this case. 
- `TypedMessage` is a combinator that speficies that message type we want to accept. We requre that whatever the message type is, it implements the `HasTypedMessage` class.
- `(:~>)` is a combinator that allows us to connect a message type with a response
- `Return` is actually an alias for `Return' 'OnCheckUnit`, described below. 

Since there are two possible ABCI messages that the router has to accomodate, `checkTx` and `deliverTx`, the router may return different values depending on the ABCI message type. For example, it's possible that the `checkTx` does not fully mimic the transaction and simply returns `()`, while the `deliverTx` message returns a value of type `Whois`. Specifically you would write

~~~ haskell ignore
type BuyNameHandler = TypeMessage BuyName :~> Return' 'OnCheckUnit Whois
~~~

or equivalently 

~~~ haskell ignore
type BuyNameHandler = TypeMessage BuyName :~> Return Whois
~~~

 Alternatively, you could write the application so that each `checkTx` ABCI message is handled in the same way as the `deliverTx` message, e.g. the both return a value of type `Whois`.

~~~ haskell ignore
type BuyNameHandler = TypeMessage BuyName :~> Return' 'OnCheckEval Whois
~~~


In the case of our actual application, all the transactions return `()` all the 

## Implementing the Handlers

Similar to the servant style, the types for the handlers must be computed from the type of the router. This requires that you understand what each of the combinators corresponds to:

- `TypedMessage` corresponds to `RoutingTx`, e.g. `TypedMessage BuyName` corresponds to `RoutingTx BuyName`
- `:~>` corrosponds to 

[Next: Module](Module.md)