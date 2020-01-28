module Tendermint.SDK.Modules.Auth
  ( AuthM
  , authModule

  , AuthEffs
  , Account(..)
  , Amount(..)
  , Coin(..)
  , CoinId(..)
  , Accounts(..)
  , getAccount
  , putAccount
  , createAccount
  , eval

  , Api
  , server

  , module Tendermint.SDK.Modules.Auth.Types
  ) where

import           Polysemy                           (Members)
import           Tendermint.SDK.Application.Module  (Module (..))
import           Tendermint.SDK.BaseApp             (BaseAppEffs, EmptyTxServer,
                                                     emptyTxServer)
import           Tendermint.SDK.Modules.Auth.Keeper
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type AuthM r = Module AuthModule EmptyTxServer Api AuthEffs r

authModule
  :: Members BaseAppEffs r
  => AuthM r
authModule = Module
  { moduleTxDeliverer = emptyTxServer
  , moduleTxChecker = emptyTxServer
  , moduleQueryServer = server
  , moduleEval = eval
  }
