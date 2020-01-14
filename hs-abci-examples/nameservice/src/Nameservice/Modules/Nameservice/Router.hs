module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (NameserviceEffs,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Polysemy                                 (Members, Sem)
import           Tendermint.SDK.BaseApp                   (BaseAppEffs, TxEffs,
                                                           incCount, withTimer)
import           Tendermint.SDK.Modules.Token             (TokenEffs)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (PreRoutedTx (..),
                                                           Tx (..))

router
  :: Members TokenEffs r
  => Members NameserviceEffs r
  => Members BaseAppEffs r
  => Members TxEffs r
  => PreRoutedTx NameserviceMessage
  -> Sem r ()
router (PreRoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       NSetName msg    -> do
         incCount "set_total"
         withTimer "set_duration_seconds" $ setName msg
       NBuyName msg    -> do
         incCount "buy_total"
         withTimer "buy_duration_seconds" $ buyName msg
       NDeleteName msg -> do
         incCount "delete_total"
         withTimer "delete_duration_seconds" $ deleteName msg
