module Tendermint.SDK.Modules.Auth
  ( AuthM
  , authModule

  , module Tendermint.SDK.Modules.Auth.Keeper
  , module Tendermint.SDK.Modules.Auth.Query
  , module Tendermint.SDK.Modules.Auth.Types
  ) where

import           Tendermint.SDK.Application.Module  (Module (..), ModuleMembers)
import           Tendermint.SDK.BaseApp             (EmptyTxServer (..))
import           Tendermint.SDK.Modules.Auth.Keeper hiding (storeKey)
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type AuthM = Module AuthModule EmptyTxServer EmptyTxServer Api AuthEffs '[]

authModule
  :: ModuleMembers AuthM r
  => AuthM r
authModule = Module
  { moduleTxDeliverer = EmptyTxServer
  , moduleTxChecker = EmptyTxServer
  , moduleQuerier = querier
  , moduleEval = eval
  }
