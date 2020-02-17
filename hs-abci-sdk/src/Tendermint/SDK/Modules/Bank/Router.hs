module Tendermint.SDK.Modules.Bank.Router
  ( MessageApi
  , messageHandlers
  ) where

import           Polysemy                             (Members, Sem)
import           Servant.API                          ((:<|>) (..))
import           Tendermint.SDK.BaseApp               ((:~>), BaseEffs, Return,
                                                       RouteTx, RoutingTx (..),
                                                       TxEffs, TypedMessage)
import           Tendermint.SDK.Modules.Auth          (AuthEffs, Coin (..))
import           Tendermint.SDK.Modules.Bank.Keeper   (BankEffs, burn, transfer)
import           Tendermint.SDK.Modules.Bank.Messages (Burn (..), Transfer (..))
import           Tendermint.SDK.Types.Message         (Msg (..))
import           Tendermint.SDK.Types.Transaction     (Tx (..))

type MessageApi =
       TypedMessage Burn :~> Return ()
  :<|> TypedMessage Transfer :~> Return ()

messageHandlers
  :: Members AuthEffs r
  => Members BankEffs r
  => Members TxEffs r
  => Members BaseEffs r
  => RouteTx MessageApi r
messageHandlers = burnH :<|> transferH

transferH
  :: Members AuthEffs r
  => Members BankEffs r
  => Members TxEffs r
  => Members BaseEffs r
  => RoutingTx Transfer
  -> Sem r ()
transferH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let Transfer{..} = msgData
      coin = Coin transferCoinId transferAmount
  in transfer transferFrom coin transferTo

burnH
  :: Members AuthEffs r
  => Members BankEffs r
  => RoutingTx Burn
  -> Sem r ()
burnH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let Burn{..} = msgData
      coin = Coin burnCoinId burnAmount
  in burn burnAddress coin
