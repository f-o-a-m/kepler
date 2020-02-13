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
import           Tendermint.SDK.BaseApp                      ((:~>), BaseEffs,
                                                              Return, RouteTx,
                                                              RoutingTx (..),
                                                              TxEffs,
                                                              TypedMessage)
import           Tendermint.SDK.Types.Message                (Msg (..))
import           Tendermint.SDK.Types.Transaction            (Tx (..))


type MessageApi =
  TypedMessage UpdateCountTx :~> Return ()

messageHandlers
  :: Member SimpleStorage r
  => Members TxEffs r
  => Members BaseEffs r
  => RouteTx MessageApi r
messageHandlers = updateCountH

updateCountH
  :: Member SimpleStorage r
  => Members TxEffs r
  => Members BaseEffs r
  => RoutingTx UpdateCountTx
  -> Sem r ()
updateCountH (RoutingTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
      UpdateCountTx{updateCountTxCount} = msgData
  in updateCount (Count updateCountTxCount)
