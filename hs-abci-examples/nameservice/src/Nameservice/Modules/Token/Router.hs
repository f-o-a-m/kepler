module Nameservice.Modules.Token.Router where

import           Nameservice.Modules.Token.Keeper   (HasTokenEff, burn, mint,
                                                     transfer, faucetAccount)
import           Nameservice.Modules.Token.Messages (Burn (..), Mint (..),
                                                     TokenMessage (..),
                                                     Transfer (..))
import           Polysemy                           (Sem)
import           Tendermint.SDK.Types.Message       (Msg (..))
import           Tendermint.SDK.Types.Transaction   (RoutedTx (..), Tx (..))

router
  :: HasTokenEff r
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
       TMint Mint{..} ->
         mint mintAddress mintAmount
