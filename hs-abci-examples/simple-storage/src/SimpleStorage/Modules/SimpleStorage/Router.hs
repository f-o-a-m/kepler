module SimpleStorage.Modules.SimpleStorage.Router
  ( router
  ) where

import           Polysemy                                    (Member, Members,
                                                              Sem)
import           SimpleStorage.Modules.SimpleStorage.Keeper  (SimpleStorage,
                                                              updateCount)
import           SimpleStorage.Modules.SimpleStorage.Message
import           SimpleStorage.Modules.SimpleStorage.Types   (Count (..))
import           Tendermint.SDK.BaseApp                      (TxEffs)
import           Tendermint.SDK.Types.Message                (Msg (..))
import           Tendermint.SDK.Types.Transaction            (RoutedTx (..),
                                                              Tx (..))


router
  :: Member SimpleStorage r
  => Members TxEffs r
  => RoutedTx SimpleStorageMessage
  -> Sem r ()
router (RoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       UpdateCount UpdateCountTx{updateCountTxCount} ->
          updateCount (Count updateCountTxCount)
