module Nameservice.Modules.Token
  (

  -- * Module
    TokenM
  , tokenModule
  -- * types
  , Address
  , Amount(..)
  , TokenError(..)
  , Transfer(..)

  -- * effects
  , Token
  , TokenEffs
  , Faucetted(..)
  , TransferEvent(..)
  , FaucetAccount(..)
  , getBalance
  , faucetAccount
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

import           Nameservice.Modules.Token.Keeper
import           Nameservice.Modules.Token.Messages
import           Nameservice.Modules.Token.Query
import           Nameservice.Modules.Token.Router
import           Nameservice.Modules.Token.Types
import           Polysemy                           (Members)
import           Tendermint.SDK.BaseApp             (BaseAppEffs)
import           Tendermint.SDK.Module              (Module (..))
import           Tendermint.SDK.Types.Address       (Address)

tokenModule
  :: Members BaseAppEffs r
  => Members TokenEffs r
  => Module "token" TokenMessage Api r
tokenModule = Module
  { moduleRouter = router
  , moduleQueryServer = server
  }
