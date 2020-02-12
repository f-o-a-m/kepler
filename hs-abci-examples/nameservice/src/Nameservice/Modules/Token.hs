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
  , Burn(..)
  , FaucetAccount(..)

  -- * effects
  , Token
  , TokenEffs
  , Faucetted(..)
  , TransferEvent(..)
  , getBalance
  , faucetAccount
  , transfer
  , mint
  , burn

  -- * interpreter
  , eval

  -- * Transaction
  , MessageApi
  , messageHandlers

  -- * Query Api
  , QueryApi
  , querier

  ) where

import           Data.Proxy
import           Nameservice.Modules.Token.Keeper
import           Nameservice.Modules.Token.Messages
import           Nameservice.Modules.Token.Query
import           Nameservice.Modules.Token.Router
import           Nameservice.Modules.Token.Types
import           Polysemy                           (Members)
import           Tendermint.SDK.Application         (Module (..))
import           Tendermint.SDK.BaseApp             (BaseEffs, TxEffs,
                                                     DefaultCheckTx (..))
import           Tendermint.SDK.Types.Address       (Address)

type TokenM r = Module "token" MessageApi MessageApi QueryApi TokenEffs r

tokenModule
  :: Members BaseEffs r
  => Members TxEffs r
  => Members TokenEffs r
  => TokenM r
tokenModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
