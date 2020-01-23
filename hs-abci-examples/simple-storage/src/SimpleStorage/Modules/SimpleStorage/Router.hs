module SimpleStorage.Modules.SimpleStorage.Router
  ( MessageApi
  , messageHandlers
  ) where

import           Polysemy                                    (Member, Members,
                                                              Sem)
import           SimpleStorage.Modules.SimpleStorage.Keeper  (SimpleStorage,
                                                              updateCount)
import           SimpleStorage.Modules.SimpleStorage.Message
import           SimpleStorage.Modules.SimpleStorage.Types   (Count (..))
import           Tendermint.SDK.BaseApp                      ((:~>),
                                                              PreRoutedTx (..),
                                                              Return,
                                                              RouteContext (..),
                                                              RouteTx, TxEffs,
                                                              TypedMessage)
import           Tendermint.SDK.Types.Message                (Msg (..))
import           Tendermint.SDK.Types.Transaction            (Tx (..))


type MessageApi =
  TypedMessage "update_count" UpdateCountTx :~> Return ()

messageHandlers
  :: Member SimpleStorage r
  => RouteTx MessageApi r 'DeliverTx
messageHandlers = updateCountH

updateCountH
  :: Member SimpleStorage r
  => Members TxEffs r
  => PreRoutedTx UpdateCountTx
  -> Sem r ()
updateCountH (PreRoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
      UpdateCountTx{updateCountTxCount} = msgData
  in updateCount (Count updateCountTxCount)
