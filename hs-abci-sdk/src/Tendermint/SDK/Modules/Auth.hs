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

import           Polysemy                           (Members)
import           Tendermint.SDK.Application.Module  (Module (..))
import           Tendermint.SDK.BaseApp             (BaseAppEffs, EmptyServer,
                                                     emptyServer)
import           Tendermint.SDK.Modules.Auth.Keeper
import           Tendermint.SDK.Modules.Auth.Query
import           Tendermint.SDK.Modules.Auth.Types

type AuthM r = Module AuthModule EmptyServer Api AuthEffs r

authModule
  :: Members BaseAppEffs r
  => AuthM r
authModule = Module
  { moduleTxServer = const emptyServer
  , moduleQueryServer = server
  , moduleEval = eval
  }
