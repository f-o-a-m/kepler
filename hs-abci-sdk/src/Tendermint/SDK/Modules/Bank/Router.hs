module Tendermint.SDK.Modules.Bank.Router where

import           Polysemy                             (Members, Sem)
import           Tendermint.SDK.BaseApp               (BaseAppEffs, TxEffs)
import qualified Tendermint.SDK.Modules.Auth          as Auth
import           Tendermint.SDK.Modules.Bank.Keeper   (BankEffs, burn,
                                                       transfer)
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
       TTransfer Transfer{..} ->
         let coin = Auth.Coin transferCoinId transferAmount
         in transfer transferFrom coin transferTo
       TBurn Burn{..} ->
         let coin = Auth.Coin burnCoinId burnAmount
         in burn burnAddress coin
