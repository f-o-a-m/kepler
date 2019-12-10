module SimpleStorage.Modules.SimpleStorage.Router
  ( router
  ) where

import           Polysemy                                    (Members, Sem)
import           Polysemy.Output                             (Output)
import           SimpleStorage.Modules.SimpleStorage.Keeper  (SimpleStorage,
                                                              putCount)
import           SimpleStorage.Modules.SimpleStorage.Message
import           SimpleStorage.Modules.SimpleStorage.Types   (Count (..))
import           Tendermint.SDK.BaseApp                      (Event)
import           Tendermint.SDK.Types.Message                (Msg (..))
import           Tendermint.SDK.Types.Transaction            (RoutedTx (..),
                                                              Tx (..))


router
  :: Members [SimpleStorage, Output Event] r
  => RoutedTx SimpleStorageMessage
  -> Sem r ()
router (RoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       UpdateCount UpdateCountTx{updateCountTxCount} ->
          putCount (Count updateCountTxCount)
