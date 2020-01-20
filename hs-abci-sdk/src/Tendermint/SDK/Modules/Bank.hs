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
  , transfer
  , mint
  , burn

  -- * interpreter
  , eval

  -- * router
  , router

  -- * Query Api
  , Api
  , server

  ) where

import           Polysemy                             (Members)
import           Tendermint.SDK.Application           (Module (..),
                                                       defaultTxChecker)
import           Tendermint.SDK.BaseApp               (BaseAppEffs)
import qualified Tendermint.SDK.Modules.Auth          as Auth
import           Tendermint.SDK.Modules.Bank.Keeper
import           Tendermint.SDK.Modules.Bank.Messages
import           Tendermint.SDK.Modules.Bank.Query
import           Tendermint.SDK.Modules.Bank.Router
import           Tendermint.SDK.Modules.Bank.Types
import           Tendermint.SDK.Types.Address         (Address)

type BankM r = Module "bank" BankMessage () Api BankEffs r

bankModule
  :: Members BaseAppEffs r
  => Members Auth.AuthEffs r
  => Members BankEffs r
  => BankM r
bankModule = Module
  { moduleTxDeliverer = router
  , moduleTxChecker = defaultTxChecker
  , moduleQueryServer = server
  , moduleEval = eval
  }
