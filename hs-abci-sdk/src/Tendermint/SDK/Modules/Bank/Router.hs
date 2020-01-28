module Nameservice.Modules.Bank.Router
  ( MessageApi
  , messageHandlers
  ) where

import           Nameservice.Modules.Bank.Keeper   (BankEffs, burn,
                                                    faucetAccount, transfer)
import           Nameservice.Modules.Bank.Messages (Burn (..), FaucetAccount,
                                                    Transfer (..))
import           Polysemy                          (Members, Sem)
import           Servant.API                       ((:<|>) (..))
import           Tendermint.SDK.BaseApp            ((:~>), BaseAppEffs, Return,
                                                    RouteContext (..), RouteTx,
                                                    RoutingTx (..), TxEffs,
                                                    TypedMessage)
import           Tendermint.SDK.Modules.Auth       (AuthEffs, Coin (..))
import           Tendermint.SDK.Types.Message      (Msg (..))
import           Tendermint.SDK.Types.Transaction  (Tx (..))

type MessageApi =
       TypedMessage Burn :~> Return ()
  :<|> TypedMessage Transfer :~> Return ()

messageHandlers
  :: Members AuthEffs r
  => Members BankEffs r
  => Members BaseAppEffs r
  => RouteTx MessageApi r 'DeliverTx
messageHandlers = burnH :<|> transferH :<|> faucetH

transferH
  :: Members AuthEffs r
  => Members BankEffs r
  => Members TxEffs r
  => Members BaseAppEffs r
  => RoutingTx Transfer
  -> Sem r ()
transferH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let Transfer{..} = msgData
  in transfer transferFrom transferAmount transferTo

burnH
  :: Members AuthEffs r
  => Members BankEffs r
  => RoutingTx Burn
  -> Sem r ()
burnH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let Burn{..} = msgData
  in burn burnAddress burnAmount
