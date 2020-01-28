module Tendermint.SDK.Modules.Bank
  (

  -- * Module
    BankM
  , bankModule
  -- * types
  , Address
  , BankError(..)
  , Transfer(..)

  -- * effects
  , BankEffs
  , TransferEvent(..)
  , getCoinBalance
  , putCoinBalance
  , transfer
  , mint
  , burn

  -- * interpreter
  , eval

  -- * transaction
  , MessageApi
  , messageHandlers

  -- * Query Api
  , QueryApi
  , server

  ) where

import           Data.Proxy
import           Polysemy                             (Members)
import           Tendermint.SDK.Application           (Module (..))
import           Tendermint.SDK.BaseApp               (BaseAppEffs,
                                                       DefaultCheckTx (..))
import qualified Tendermint.SDK.Modules.Auth          as Auth
import           Tendermint.SDK.Modules.Bank.Keeper
import           Tendermint.SDK.Modules.Bank.Messages
import           Tendermint.SDK.Modules.Bank.Query
import           Tendermint.SDK.Modules.Bank.Router
import           Tendermint.SDK.Modules.Bank.Types
import           Tendermint.SDK.Types.Address         (Address)

type BankM r = Module "bank" MessageApi QueryApi BankEffs r

bankModule
  :: Members BaseAppEffs r
  => Members Auth.AuthEffs r
  => Members BankEffs r
  => BankM r
bankModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQueryServer = server
  , moduleEval = eval
  }
