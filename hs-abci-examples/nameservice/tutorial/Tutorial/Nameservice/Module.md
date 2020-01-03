# Module

## Tutorial.Nameservice.Module

At this point we can collect the relevant pieces to form the Nameservice module:

~~~ haskell
module Tutorial.Nameservice.Module where

import Nameservice.Modules.Nameservice.Keeper (NameserviceEffs, eval)
import Nameservice.Modules.Nameservice.Messages (NameserviceMessage)
import Nameservice.Modules.Nameservice.Query (Api, server)
import Nameservice.Modules.Nameservice.Router (router)
import Nameservice.Modules.Nameservice.Types (NameserviceModuleName)
import Nameservice.Modules.Token                (TokenEffs)
import Polysemy                                 (Members)
import Tendermint.SDK.Application               (Module (..),
                                                 defaultTxChecker)
import Tendermint.SDK.BaseApp                   (BaseAppEffs)

-- a convenient type alias
type NameserviceM r =
  Module NameserviceModuleName NameserviceMessage () Api NameserviceEffs r

nameserviceModule
  :: Members BaseAppEffs r
  => Members TokenEffs r
  => Members NameserviceEffs r
  => NameserviceM r
nameserviceModule = Module
  { moduleTxDeliverer = router
  , moduleTxChecker = defaultTxChecker
  , moduleQueryServer = server
  , moduleEval = eval
  }
~~~

We are using `defaultTxChecker` as our transaction checker, which is a static message validator defined as 

~~~ haskell ignore
defaultTxChecker
  :: Member (Error AppError) r
  => ValidateMessage msg
  => RoutedTx msg
  -> Sem r ()
defaultTxChecker (RoutedTx Tx{txMsg}) =
  case validateMessage txMsg of
    V.Failure err ->
      throwSDKError . MessageValidation . map formatMessageSemanticError $ err
    V.Success _ -> pure ()
~~~

This means that we are only doing static validation, meaning that we're not interested in checking message validitity against the database. This is reflected in the return type for the checker `Sem r ()`. If you want to add custom checking, you may write a custom checker for your module. 

Note the constraints on the module's effects `r`:

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
