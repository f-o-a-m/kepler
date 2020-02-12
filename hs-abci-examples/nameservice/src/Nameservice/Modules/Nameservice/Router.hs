module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (NameserviceEffs,
                                                           buyName, deleteName,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (BuyName, DeleteName,
                                                           SetName)
import           Nameservice.Modules.Token                (TokenEffs)
import           Polysemy                                 (Members, Sem)
import           Servant.API                              ((:<|>) (..))
import           Tendermint.SDK.BaseApp                   ((:~>), BaseEffs,
                                                           Return, RouteTx,
                                                           RoutingTx (..),
                                                           TxEffs, TypedMessage,
                                                           incCount, withTimer)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (Tx (..))



type MessageApi =
       TypedMessage BuyName :~> Return ()
  :<|> TypedMessage SetName :~> Return ()
  :<|> TypedMessage DeleteName :~> Return ()

messageHandlers
  :: Members BaseEffs r
  => Members TokenEffs r
  => Members TxEffs r
  => Members NameserviceEffs r
  => RouteTx MessageApi r
messageHandlers = buyNameH :<|> setNameH :<|> deleteNameH

buyNameH
  :: Members BaseEffs r
  => Members TxEffs r
  => Members TokenEffs r
  => Members NameserviceEffs r
  => RoutingTx BuyName
  -> Sem r ()
buyNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "buy_total"
  withTimer "buy_duration_seconds" $ buyName msgData

setNameH
  :: Members BaseEffs r
  => Members TxEffs r
  => Members NameserviceEffs r
  => RoutingTx SetName
  -> Sem r ()
setNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "set_total"
  withTimer "set_duration_seconds" $ setName msgData

deleteNameH
  :: Members BaseEffs r
  => Members TxEffs r
  => Members TokenEffs r
  => Members NameserviceEffs r
  => RoutingTx DeleteName
  -> Sem r ()
deleteNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "delete_total"
  withTimer "delete_duration_seconds" $ deleteName msgData
