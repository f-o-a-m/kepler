module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (NameserviceEffs,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (TokenEffs)
import           Polysemy                                 (Members, Sem)
import           Tendermint.SDK.BaseApp                   (BaseAppEffs)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (RoutedTx (..),
                                                           Tx (..))

router
  :: Members TokenEffs r
  => Members NameserviceEffs r
  => Members BaseAppEffs r
  => RoutedTx NameserviceMessage
  -> Sem r ()
router (RoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       NSetName msg    -> setName msg
       NBuyName msg    -> buyName msg
       NDeleteName msg -> deleteName msg
