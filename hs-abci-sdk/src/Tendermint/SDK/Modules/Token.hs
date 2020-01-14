module Tendermint.SDK.Modules.Token
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

import           Polysemy                              (Members)
import           Tendermint.SDK.Application            (Module (..),
                                                        defaultTxChecker)
import           Tendermint.SDK.BaseApp                (BaseAppEffs)
import           Tendermint.SDK.Modules.Token.Keeper
import           Tendermint.SDK.Modules.Token.Messages
import           Tendermint.SDK.Modules.Token.Query
import           Tendermint.SDK.Modules.Token.Router
import           Tendermint.SDK.Modules.Token.Types
import           Tendermint.SDK.Types.Address          (Address)

type TokenM r = Module "token" TokenMessage () Api TokenEffs r

tokenModule
  :: Members BaseAppEffs r
  => Members TokenEffs r
  => TokenM r
tokenModule = Module
  { moduleTxDeliverer = router
  , moduleTxChecker = defaultTxChecker
  , moduleQueryServer = server
  , moduleEval = eval
  }
