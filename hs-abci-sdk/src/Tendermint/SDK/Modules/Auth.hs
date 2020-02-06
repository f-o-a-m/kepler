module Tendermint.SDK.Modules.Auth
  ( AuthM
  , authModule

  , AuthEffs
  , Account(..)
  , Accounts(..)
  , getAccount
  , putAccount
  , createAccount
  , eval

  , Api
  , server

  , module Tendermint.SDK.Modules.Auth.Types
  ) where

import           Tendermint.SDK.Application.Module  (Module (..))
import           Tendermint.SDK.BaseApp             (EmptyTxServer,
                                                     emptyTxServer)
import           Tendermint.SDK.Modules.Auth.Keeper
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type AuthM r = Module AuthModule EmptyTxServer Api AuthEffs r

authModule :: AuthM r
authModule = Module
  { moduleTxDeliverer = emptyTxServer
  , moduleTxChecker = emptyTxServer
  , moduleQueryServer = server
  , moduleEval = eval
  }
