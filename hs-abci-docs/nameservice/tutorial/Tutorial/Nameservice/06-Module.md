---
title: Nameservice: Module
---

# Module

## Tutorial.Nameservice.Module

At this point we can collect the relevant pieces to form the Nameservice module:

~~~ haskell
module Tutorial.Nameservice.Module where

import Nameservice.Modules.Nameservice.Keeper (NameserviceEffs, eval)
import Nameservice.Modules.Nameservice.Query (QueryApi, server)
import Nameservice.Modules.Nameservice.Router (MessageApi, messageHandlers)
import Nameservice.Modules.Nameservice.Types (NameserviceModuleName)
import Nameservice.Modules.Token                (TokenEffs)
import Polysemy                                 (Members)
import Data.Proxy
import Tendermint.SDK.Application               (Module (..))
import           Tendermint.SDK.BaseApp       (BaseAppEffs,
                                               DefaultCheckTx (..))

-- a convenient type alias
type NameserviceM r =
  Module NameserviceModuleName MessageApi QueryApi NameserviceEffs r

nameserviceModule
  :: Members BaseAppEffs r
  => Members TokenEffs r
  => Members NameserviceEffs r
  => NameserviceM r
nameserviceModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQueryServer = server
  , moduleEval = eval
  }

~~~

Here We are using `defaultCheckTx` as our transaction checker, which is a static message validator defined that respons to any message with the following handler:

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
2. The return type is marked with `OnCheckUnit`, meaning that `()` is returned for any `checkTx` ABCI message.

To generate a router for which every transaction has these properties, we used the `defaultCheckTx` type class method

Note the constraints on `r`:

~~~ haskell ignore
...
  :: Members BaseAppEffs r
  => Members TokenEffs r
  => Members NameserviceEffs r
...
~~~

This is saying that we can run this module in any context for which `r` has the effects from `BaseApp`, `Token`, and `Nameservice`. This is how we explicitly declare module dependencies, by using the constraint system.

Other than that, there is nothing really to note. We are just collecting the pieces we have already defined in one place.

[Next: Application](Application.md)
