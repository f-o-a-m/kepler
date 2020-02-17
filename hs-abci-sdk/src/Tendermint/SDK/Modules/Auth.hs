module Tendermint.SDK.Modules.Auth
  ( AuthM
  , authModule

  , module Tendermint.SDK.Modules.Auth.Keeper
  , module Tendermint.SDK.Modules.Auth.Query
  , module Tendermint.SDK.Modules.Auth.Types
  ) where

import           Polysemy                           (Member, Members)
import           Tendermint.SDK.Application.Module  (Module (..))
import           Tendermint.SDK.BaseApp             (BaseEffs,
                                                     EmptyTxServer (..),
                                                     ReadStore)
import           Tendermint.SDK.Modules.Auth.Keeper hiding (storeKey)
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type AuthM r = Module AuthModule EmptyTxServer EmptyTxServer Api AuthEffs r

authModule
  :: Members BaseEffs r
  => Member ReadStore r
  => AuthM r
authModule = Module
  { moduleTxDeliverer = EmptyTxServer
  , moduleTxChecker = EmptyTxServer
  , moduleQuerier = querier
  , moduleEval = eval
  }
