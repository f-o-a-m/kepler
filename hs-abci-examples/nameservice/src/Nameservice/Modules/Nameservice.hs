module Nameservice.Modules.Nameservice

  (
    -- * Module
    NameserviceM
  , nameserviceModule

    -- * types
  , Name(..)
  , Whois (..)
  , NameserviceError(..)
  , NameserviceMessage(..)
  , NameClaimed(..)
  , NameRemapped(..)
  , NameDeleted(..)
  , BuyName(..)
  , SetName(..)
  , DeleteName(..)

  -- * effects
  , NameserviceEffs
  , Faucetted(..)
  , FaucetAccount(..)
  , getWhois
  , buyName
  , setName
  , deleteName
  , faucetAccount

  -- * interpreter
  , eval

  -- * message router
  , router

  -- * query API
  , Api
  , server

  ) where

import           Nameservice.Modules.Nameservice.Keeper
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Query
import           Nameservice.Modules.Nameservice.Router
import           Nameservice.Modules.Nameservice.Types
import           Polysemy                                 (Members)
import           Tendermint.SDK.Application               (Module (..),
                                                           defaultTxChecker)
import           Tendermint.SDK.BaseApp                   (BaseAppEffs)
import           Tendermint.SDK.Modules.Bank              (BankEffs)

type NameserviceM r =
  Module "nameservice" NameserviceMessage () Api NameserviceEffs r

nameserviceModule
  :: Members BaseAppEffs r
  => Members BankEffs r
  => Members NameserviceEffs r
  => NameserviceM r
nameserviceModule = Module
  { moduleTxDeliverer = router
  , moduleTxChecker = defaultTxChecker
  , moduleQueryServer = server
  , moduleEval = eval
  }
