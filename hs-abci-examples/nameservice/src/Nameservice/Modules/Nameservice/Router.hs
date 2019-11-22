module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (HasNameserviceEff,
                                                           buyName, deleteName,
                                                           faucetAccount,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (HasTokenEff)
import           Polysemy                                 (Sem)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (RoutedTx (..),
                                                           Tx (..))

router
  :: HasTokenEff r
  => HasNameserviceEff r
  => RoutedTx NameserviceMessage
  -> Sem r ()
router (RoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       NSetName msg       -> setName msg
       NBuyName msg       -> buyName msg
       NDeleteName msg    -> deleteName msg
       NFaucetAccount msg -> faucetAccount msg
