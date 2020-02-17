---
title: Nameservice - Module
---

# Module

## Tutorial.Nameservice.Module

At this point we can collect the relevant pieces to form the Nameservice module:

~~~ haskell
module Tutorial.Nameservice.Module where

import Nameservice.Modules.Nameservice.Keeper (NameserviceEffs, eval)
import Nameservice.Modules.Nameservice.Query (QueryApi, querier)
import Nameservice.Modules.Nameservice.Router (MessageApi, messageHandlers)
import Nameservice.Modules.Nameservice.Types (NameserviceModuleName)
import Polysemy                                 (Members)
import Tendermint.SDK.Application               (Module (..))
import Tendermint.SDK.BaseApp (BaseEffs, TxEffs, DefaultCheckTx (..))
import Tendermint.SDK.Modules.Bank                (BankEffs)
import Data.Proxy
import Tendermint.SDK.Modules.Auth                (AuthEffs)

-- a convenient type alias
type NameserviceM r =
  Module NameserviceModuleName MessageApi MessageApi QueryApi NameserviceEffs r

nameserviceModule
  :: Members BaseEffs r
  => Members BankEffs r
  => Members TxEffs r
  => Members AuthEffs r
  => Members NameserviceEffs r
  => NameserviceM r
nameserviceModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }

~~~

Here We are using `defaultCheckTx` as our transaction checker, which is a static, message validating handler defined as:

~~~ haskell ignore
defaultCheckTxHandler
  :: Member (Error AppError) r
  => ValidateMessage msg
  => RoutingTx msg
  -> Sem r ()
defaultCheckTxHandler(RoutingTx Tx{txMsg}) =
  case validateMessage txMsg of
    V.Failure err ->
      throwSDKError . MessageValidation . map formatMessageSemanticError $ err
    V.Success _ -> pure ()
~~~

Note that this checker can be used to implement any transaction for which
1. The message accepted by the router has a `ValidateMessage` instance
2. The return type in the serve type is `Return ()`

To generate a server for which every transaction has these properties, we used the `defaultCheckTx` type class method on the `MessageApi` type. This will generate a server of type `VoidReturn MessageApi`, which has the exact same shape as `MessageApi` just will all the return values changed to `Return ()`. In this paricular case all handlers for `MessageApi` already return `()`, so we have `MessageApi ~ VoidReturn MessageApi` and there's no need to use the `VoidReturn` family in the module type.

Note the constraints on `r` in the Module's type:

~~~ haskell ignore
...
  :: Members BaseEffs r
  => Members BankEffs r
  => Members NameserviceEffs r
...
~~~

This is saying that we can run this module in any context for which `r` has the effects from `BaseApp`, `Bank`, and `Nameservice`. This is how we explicitly declare module dependencies, by using the constraint system.

Other than that, there is nothing really to note. We are just collecting the pieces we have already defined in one place.
