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
  , getWhois
  , buyName
  , setName
  , deleteName

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
import           Nameservice.Modules.Token                (TokenEffs)
import           Polysemy                                 (Members)
import           Tendermint.SDK.Application               (Module (..),
                                                           defaultTxChecker)
import           Tendermint.SDK.BaseApp                   (BaseAppEffs)

type NameserviceM r = Module "nameservice" NameserviceMessage Api r

nameserviceModule
  :: Members BaseAppEffs r
  => Members TokenEffs r
  => Members NameserviceEffs r
  => NameserviceM r
nameserviceModule = Module
  { moduleTxDeliverer = router
  , moduleTxChecker = defaultTxChecker
  , moduleQueryServer = server
  }
