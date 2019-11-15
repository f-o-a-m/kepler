module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (HasNameserviceEff,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (HasTokenEff)
import           Polysemy                                 (Members, Sem)
import           Polysemy.Error                           (Error)
import           Tendermint.SDK.Auth                      (AuthError, Msg (..),
                                                           Transaction, Tx (..))
import           Tendermint.SDK.Errors                    (AppError)
import qualified Tendermint.SDK.TxRouter                  as R

router
  :: Members [Error AppError, Error AuthError] r
  => HasTokenEff r
  => HasNameserviceEff r
  => Transaction
  -> Sem r ()
router = R.router handler
  where
    handler
      :: HasTokenEff r
      => HasNameserviceEff r
      => R.Handler r NameserviceMessage
    handler Tx{txMsg} =
      let Msg{msgData=msg} = txMsg
      in case msg of
           NSetName txMsg    -> setName txMsg
           NBuyName txMsg    -> buyName txMsg
           NDeleteName txMsg -> deleteName txMsg
