module SimpleStorage.Modules.SimpleStorage.Router
  ( MessageApi
  , messageHandlers
  ) where

import           Polysemy                                    (Member, Sem)
import           SimpleStorage.Modules.SimpleStorage.Keeper  (SimpleStorageKeeper,
                                                              updateCount)
import           SimpleStorage.Modules.SimpleStorage.Message
import           SimpleStorage.Modules.SimpleStorage.Types   (Count (..))
import           Tendermint.SDK.BaseApp                      ((:~>), Return,
                                                              RouteTx,
                                                              RoutingTx (..),
                                                              TypedMessage)
import           Tendermint.SDK.Types.Message                (Msg (..))
import           Tendermint.SDK.Types.Transaction            (Tx (..))


type MessageApi =
  TypedMessage UpdateCountTx :~> Return ()

messageHandlers
  :: Member SimpleStorageKeeper r
  => RouteTx MessageApi r
messageHandlers = updateCountH

updateCountH
  :: Member SimpleStorageKeeper r
  => RoutingTx UpdateCountTx
  -> Sem r ()
updateCountH (RoutingTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
      UpdateCountTx{updateCountTxCount} = msgData
  in updateCount (Count updateCountTxCount)
