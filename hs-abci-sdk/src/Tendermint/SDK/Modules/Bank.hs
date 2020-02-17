module Tendermint.SDK.Modules.Bank
  (
  -- * Module
    BankM
  , bankModule

  , module           Tendermint.SDK.Modules.Bank.Keeper
  , module           Tendermint.SDK.Modules.Bank.Messages
  , module           Tendermint.SDK.Modules.Bank.Query
  , module           Tendermint.SDK.Modules.Bank.Router
  , module           Tendermint.SDK.Modules.Bank.Types

  ) where

import           Data.Proxy
import           Polysemy                             (Members)
import           Tendermint.SDK.Application           (Module (..))
import           Tendermint.SDK.BaseApp               (BaseEffs,
                                                       DefaultCheckTx (..),
                                                       TxEffs)
import qualified Tendermint.SDK.Modules.Auth          as Auth
import           Tendermint.SDK.Modules.Bank.Keeper
import           Tendermint.SDK.Modules.Bank.Messages
import           Tendermint.SDK.Modules.Bank.Query
import           Tendermint.SDK.Modules.Bank.Router
import           Tendermint.SDK.Modules.Bank.Types

type BankM r = Module "bank" MessageApi MessageApi QueryApi BankEffs r

bankModule
  :: Members BaseEffs r
  => Members Auth.AuthEffs r
  => Members BankEffs r
  => Members TxEffs r
  => BankM r
bankModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
