module Nameservice.Modules.Token
  (

  -- * Module
    TokenM
  , tokenModule
  -- * types
  , Address
  , Amount(..)
  , TokenException(..)
  , Transfer(..)

  -- * effects
  , Token
  , TokenEffR
  , HasTokenEff
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
import           Tendermint.SDK.BaseApp             (HasBaseAppEff)
import           Tendermint.SDK.Module              (Module (..))
import           Tendermint.SDK.Types.Address       (Address)

type TokenM r = Module "token" TokenMessage Api r

tokenModule
  :: HasBaseAppEff r
  => HasTokenEff r
  => TokenM r
tokenModule = Module
  { moduleRouter = router
  , moduleQueryServer = server
  }
