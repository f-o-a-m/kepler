module Nameservice.Modules.Nameservice

  (
    -- * Module
    NameserviceM
  , nameserviceModule

    -- * types
  , Name(..)
  , Whois (..)
  , NameserviceError(..)
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
  , MessageApi
  , messageHandlers

  -- * query API
  , QueryApi
  , server

  ) where

import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Query
import           Nameservice.Modules.Nameservice.Router
import           Nameservice.Modules.Nameservice.Types
import           Nameservice.Modules.Token                (TokenEffs)
import           Polysemy                                 (Members)
import           Tendermint.SDK.Application               (Module (..))
import           Tendermint.SDK.BaseApp                   (BaseAppEffs,
                                                           DefaultCheckTx (..))

type NameserviceM r =
  Module "nameservice" MessageApi QueryApi NameserviceEffs r

nameserviceModule
  :: Members BaseAppEffs r
  => Members TokenEffs r
  => Members NameserviceEffs r
  => NameserviceM r
nameserviceModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQueryServer = server
  , moduleEval = eval
  }
