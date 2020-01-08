module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (NameserviceEffs,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (TokenEffs)
import           Polysemy                                 (Members, Sem)
import           Tendermint.SDK.BaseApp                   (BaseAppEffs, TxEffs,
                                                           incCount, withTimer)
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
         incCount "count_set"
         withTimer "histogram_set" $ setName msg
       NBuyName msg    -> do
         incCount "count_buy"
         withTimer "histogram_buy" $ buyName msg
       NDeleteName msg -> do
         incCount "count_delete"
         withTimer "histogram_delete" $ deleteName msg
