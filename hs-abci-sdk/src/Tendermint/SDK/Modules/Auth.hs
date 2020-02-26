module Tendermint.SDK.Modules.Auth
  ( Auth
  , authModule

  , module Tendermint.SDK.Modules.Auth.Keeper
  , module Tendermint.SDK.Modules.Auth.Query
  , module Tendermint.SDK.Modules.Auth.Types
  ) where

import           Polysemy                           (Members)
import           Tendermint.SDK.Application.Module  (Module (..), ModuleEffs)
import           Tendermint.SDK.BaseApp             (EmptyTxServer (..))
import           Tendermint.SDK.Modules.Auth.Keeper hiding (store)
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type Auth =
  Module AuthName EmptyTxServer EmptyTxServer Api AuthEffs '[]

authModule
  :: Members (ModuleEffs Auth) r
  => Auth r
authModule = Module
  { moduleTxDeliverer = EmptyTxServer
  , moduleTxChecker = EmptyTxServer
  , moduleQuerier = querier
  , moduleEval = eval
  }
