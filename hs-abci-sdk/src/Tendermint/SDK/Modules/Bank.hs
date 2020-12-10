module Tendermint.SDK.Modules.Bank
  (
  -- * Module
    Bank
  , bankModule

  , module           Tendermint.SDK.Modules.Bank.Keeper
  , module           Tendermint.SDK.Modules.Bank.Messages
  , module           Tendermint.SDK.Modules.Bank.Query
  , module           Tendermint.SDK.Modules.Bank.Router
  , module           Tendermint.SDK.Modules.Bank.Types

  ) where

import           Data.Proxy
import           Polysemy                             (Members)
import           Tendermint.SDK.Application           (Module (..), ModuleEffs)
import           Tendermint.SDK.BaseApp               (DefaultCheckTx (..))
import qualified Tendermint.SDK.Modules.Auth          as Auth
import           Tendermint.SDK.Modules.Bank.Keeper
import           Tendermint.SDK.Modules.Bank.Messages
import           Tendermint.SDK.Modules.Bank.Query
import           Tendermint.SDK.Modules.Bank.Router
import           Tendermint.SDK.Modules.Bank.Types

type Bank =
  Module BankName MessageApi MessageApi QueryApi BankEffs '[Auth.Auth]

bankModule
  :: Members (ModuleEffs Bank) r
  => Bank r
bankModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
