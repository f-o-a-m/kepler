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
  , querier

  , module Tendermint.SDK.Modules.Auth.Types
  ) where

import           Polysemy                           (Member, Members)
import           Tendermint.SDK.Application.Module  (Module (..))
import           Tendermint.SDK.BaseApp             (BaseAppEffs,
                                                     EmptyTxServer (..),
                                                     ReadStore)
import           Tendermint.SDK.Modules.Auth.Keeper
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type AuthM r = Module AuthModule EmptyTxServer EmptyTxServer Api AuthEffs r

authModule
  :: Members BaseAppEffs r
  => Member ReadStore r
  => AuthM r
authModule = Module
  { moduleTxDeliverer = EmptyTxServer
  , moduleTxChecker = EmptyTxServer
  , moduleQuerier = querier
  , moduleEval = eval
  }
