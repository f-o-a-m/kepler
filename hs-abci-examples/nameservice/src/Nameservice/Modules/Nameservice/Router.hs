module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (HasNameserviceEff,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (HasTokenEff)
import           Polysemy                                 (Members, Sem)
import           Polysemy.Error                           (Error, throw)
import           Tendermint.SDK.Auth                      (AuthError, Msg (..),
                                                           Transaction, Tx (..),
                                                           formatWireParseError,
                                                           parseTx)
import           Tendermint.SDK.Errors                    (SDKError (..))

routerMsg
  :: Members [Error SDKError, Error AuthError] r
  => Transaction
  -> Sem r (Tx NameserviceMessage)
routerMsg tx = do
  eRes <- parseTx tx
  case eRes of
    Left err  -> throw @SDKError (ParseError $ formatWireParseError err)
    Right res -> return res

routerHandler
  :: HasTokenEff r
  => HasNameserviceEff r
  => Tx NameserviceMessage
  -> Sem r ()
routerHandler Tx{txMsg} =
  let Msg{msgData=msg} = txMsg
  in case msg of
       SetName txMsg    -> setName txMsg
       BuyName txMsg    -> buyName txMsg
       DeleteName txMsg -> deleteName txMsg

router
  :: Members [Error SDKError, Error AuthError] r
  => HasTokenEff r
  => HasNameserviceEff r
  => Transaction
  -> Sem r ()
router tx = routerMsg tx >>= routerHandler
