module Tendermint.SDK.Modules.Bank.Router
  ( MessageApi
  , messageHandlers
  ) where

import           Polysemy                             (Members, Sem)
import           Servant.API                          ((:<|>) (..))
import           Tendermint.SDK.BaseApp               ((:~>), Return, RouteTx,
                                                       RoutingTx (..),
                                                       TypedMessage)
import           Tendermint.SDK.Modules.Auth          (Coin (..))
import           Tendermint.SDK.Modules.Bank.Keeper   (BankEffs, burn, transfer)
import           Tendermint.SDK.Modules.Bank.Messages
import           Tendermint.SDK.Types.Message         (Msg (..))
import           Tendermint.SDK.Types.Transaction     (Tx (..))

type MessageApi =
       TypedMessage BurnMsg :~> Return ()
  :<|> TypedMessage TransferMsg :~> Return ()

messageHandlers
  ::Members BankEffs r
  => RouteTx MessageApi r
messageHandlers = burnH :<|> transferH

transferH
  :: Members BankEffs r
  => RoutingTx TransferMsg
  -> Sem r ()
transferH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let TransferMsg{..} = msgData
      coin = Coin transferCoinId transferAmount
  in transfer transferFrom coin transferTo

burnH
  :: Members BankEffs r
  => RoutingTx BurnMsg
  -> Sem r ()
burnH (RoutingTx Tx{txMsg=Msg{msgData}}) =
  let BurnMsg{..} = msgData
      coin = Coin burnCoinId burnAmount
  in burn burnAddress coin
