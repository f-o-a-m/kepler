module Tendermint.SDK.Modules.Auth
  ( AuthM
  , authModule

  , AuthEffs
  , Accounts(..)
  , getAccount
  , putAccount
  , eval

  , Api
  , server

  , module Tendermint.SDK.Modules.Auth.Types
  ) where

import           Data.Void
import           Polysemy                           (Members)
import           Tendermint.SDK.Application.Module  (Module (..), voidRouter)
import           Tendermint.SDK.BaseApp             (BaseAppEffs)
import           Tendermint.SDK.Modules.Auth.Keeper
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type AuthM r = Module AuthModule Void Void Api AuthEffs r

authModule
  :: Members BaseAppEffs r
  => AuthM r
authModule = Module
  { moduleTxDeliverer = voidRouter
  , moduleTxChecker = voidRouter
  , moduleQueryServer = server
  , moduleEval = eval
  }
