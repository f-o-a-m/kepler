module Nameservice.Modules.Token.Router
  ( MessageApi
  , messageHandlers

  ) where

import           Nameservice.Modules.Token.Keeper   (TokenEffs, burn,
                                                     faucetAccount, transfer)
import           Nameservice.Modules.Token.Messages (Burn (..), FaucetAccount,
                                                     Transfer (..))
import           Polysemy                           (Members, Sem)
import           Servant.API                        ((:<|>) (..))
import           Tendermint.SDK.BaseApp             ((:~>), BaseAppEffs, Return,
                                                     RouteContext (..), RouteTx,
                                                     RoutingTx (..), TxEffs,
                                                     TypedMessage)
import           Tendermint.SDK.Types.Message       (Msg (..))
import           Tendermint.SDK.Types.Transaction   (Tx (..))

type MessageApi =
       TypedMessage "Burn" Burn :~> Return ()
  :<|> TypedMessage "Transfer" Transfer :~> Return ()
  :<|> TypedMessage "FaucetAccount" FaucetAccount :~> Return ()

messageHandlers
  :: Members TokenEffs r
  => Members BaseAppEffs r
  => RouteTx MessageApi r 'DeliverTx
messageHandlers = burnH :<|> transferH :<|> faucetH

transferH
  :: Members TokenEffs r
  => Members TxEffs r
  => Members BaseAppEffs r
  => RoutingTx Transfer
  -> Sem r ()
transferH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let Transfer{..} = msgData
  in transfer transferFrom transferAmount transferTo

burnH
  :: Members TokenEffs r
  => RoutingTx Burn
  -> Sem r ()
burnH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let Burn{..} = msgData
  in burn burnAddress burnAmount

faucetH
  :: Members TokenEffs r
  => Members TxEffs r
  => Members BaseAppEffs r
  => RoutingTx FaucetAccount
  -> Sem r ()
faucetH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  faucetAccount msgData
