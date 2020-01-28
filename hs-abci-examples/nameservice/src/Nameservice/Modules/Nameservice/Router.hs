module Nameservice.Modules.Nameservice.Router where

import           Nameservice.Modules.Nameservice.Keeper   (NameserviceEffs,
                                                           buyName, deleteName,
                                                           faucetAccount,
                                                           setName)
import           Nameservice.Modules.Nameservice.Messages (BuyName, DeleteName,
                                                           SetName)
import           Polysemy                                 (Members, Sem)
import           Servant.API                              ((:<|>) (..))
import           Tendermint.SDK.BaseApp                   ((:~>), BaseAppEffs,
                                                           Return,
                                                           RouteContext (..),
                                                           RouteTx,
                                                           RoutingTx (..),
                                                           TxEffs, TypedMessage,
                                                           incCount, withTimer)
import           Tendermint.SDK.Modules.Auth              (AuthEffs)
import           Tendermint.SDK.Modules.Bank              (BankEffs)
import           Tendermint.SDK.Types.Message             (Msg (..))
import           Tendermint.SDK.Types.Transaction         (Tx (..))


type MessageApi =
       TypedMessage BuyName :~> Return ()
  :<|> TypedMessage SetName :~> Return ()
  :<|> TypedMessage DeleteName :~> Return ()

messageHandlers
  :: Members BaseAppEffs r
  => Members BankEffs r
  => Members NameserviceEffs r
  => RouteTx MessageApi r 'DeliverTx
messageHandlers = buyNameH :<|> setNameH :<|> deleteNameH

buyNameH
  :: Members BaseAppEffs r
  => Members TxEffs r
  => Members BankEffs r
  => Members NameserviceEffs r
  => RoutingTx BuyName
  -> Sem r ()
buyNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "buy_total"
  withTimer "buy_duration_seconds" $ buyName msgData

setNameH
  :: Members BaseAppEffs r
  => Members TxEffs r
  => Members NameserviceEffs r
  => RoutingTx SetName
  -> Sem r ()
setNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "set_total"
  withTimer "set_duration_seconds" $ setName msgData

deleteNameH
  :: Members BaseAppEffs r
  => Members TxEffs r
  => Members BankEffs r
  => Members NameserviceEffs r
  => RoutingTx DeleteName
  -> Sem r ()
deleteNameH (RoutingTx Tx{txMsg=Msg{msgData}}) = do
  incCount "delete_total"
  withTimer "delete_duration_seconds" $ deleteName msgData
