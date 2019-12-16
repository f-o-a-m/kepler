module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (NameserviceEffs,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (NameserviceMessage (..))
import           Nameservice.Modules.Token                (TokenEffs)
import           Polysemy                                 (Members, Sem)
import           Tendermint.SDK.BaseApp                   (BaseAppEffs,
                                                           incCount, withTimer)
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
       NSetName msg    -> do
         incCount "count_set"
         _ <- withTimer "histogram_set" $ setName msg
         pure ()
       NBuyName msg    -> do
         incCount "count_buy"
         _ <- withTimer "histogram_buy" $ buyName msg
         pure ()
       NDeleteName msg -> do
         incCount "count_delete"
         _ <- withTimer "histogram_delete" $ deleteName msg
         pure ()
