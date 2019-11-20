module Nameservice.Modules.Nameservice.Router where

import           Data.ByteString                          (ByteString)
import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper   (HasNameserviceEff,
                                                           buyName, deleteName,
                                                           faucetAccount,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (HasTokenEff)
import           Polysemy                                 (Members, Sem)
import           Polysemy.Error                           (Error)
import           Tendermint.SDK.Auth                      (AuthError)
import           Tendermint.SDK.Codec                     (HasCodec (..))
import           Tendermint.SDK.Errors                    (AppError,
                                                           SDKError (..),
                                                           throwSDKError)
import qualified Tendermint.SDK.TxRouter                  as R
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (RoutedTx (..),
                                                           Tx (..))

router
  :: HasTokenEff r
  => HasNameserviceEff r
  => RoutedTx NameserviceMessage
  -> Sem r ()
router (RoutedTx Tx{txMsg}) =
  let Msg{msgData=msg} = txMsg
  in case msg of
       NSetName txMsg       -> setName txMsg
       NBuyName txMsg       -> buyName txMsg
       NDeleteName txMsg    -> deleteName txMsg
       NFaucetAccount txMsg -> faucetAccount txMsg
