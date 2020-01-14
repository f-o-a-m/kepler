module Tendermint.SDK.Modules.Bank.Router where

import           Polysemy                             (Members, Sem)
import           Tendermint.SDK.BaseApp               (BaseAppEffs, TxEffs)
import           Tendermint.SDK.Modules.Bank.Keeper   (BankEffs, burn,
                                                       faucetAccount, transfer)
import           Tendermint.SDK.Modules.Bank.Messages (BankMessage (..),
                                                       Burn (..), Transfer (..))
import           Tendermint.SDK.Types.Message         (Msg (..))
import           Tendermint.SDK.Types.Transaction     (PreRoutedTx (..),
                                                       Tx (..))

router
  :: Members BankEffs r
  => Members BaseAppEffs r
  => Members TxEffs r
  => PreRoutedTx BankMessage
  -> Sem r ()
router (PreRoutedTx Tx{txMsg}) =
  let Msg{msgData} = txMsg
  in case msgData of
       TFaucetAccount faucet ->
         faucetAccount faucet
       TTransfer Transfer{..} ->
         transfer transferFrom transferAmount transferTo
       TBurn Burn{..} ->
         burn burnAddress burnAmount
