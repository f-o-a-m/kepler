module Nameservice.Modules.Token.Router where

import           Nameservice.Modules.Token.Keeper   (TokenEffs, burn,
                                                     faucetAccount, transfer)
import           Nameservice.Modules.Token.Messages (Burn (..),
                                                     TokenMessage (..),
                                                     Transfer (..))
import           Polysemy                           (Member, Members, Sem)
import           Polysemy.Output                    (Output)
import           Tendermint.SDK.Events              (Event)
import           Tendermint.SDK.Types.Message       (Msg (..))
import           Tendermint.SDK.Types.Transaction   (RoutedTx (..), Tx (..))

router
  :: Members TokenEffs r
  => Member (Output Event) r
  => RoutedTx TokenMessage
  -> Sem r ()
router (RoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       TFaucetAccount faucet ->
         faucetAccount faucet
       TTransfer Transfer{..} ->
         transfer transferFrom transferAmount transferTo
       TBurn Burn{..} ->
         burn burnAddress burnAmount
