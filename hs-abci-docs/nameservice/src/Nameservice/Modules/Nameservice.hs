module Nameservice.Modules.Nameservice

  (
    -- * Module
    Nameservice
  , nameserviceModule
  , module           Nameservice.Modules.Nameservice.Keeper
  , module           Nameservice.Modules.Nameservice.Messages
  , module           Nameservice.Modules.Nameservice.Store
  , module           Nameservice.Modules.Nameservice.Query
  , module           Nameservice.Modules.Nameservice.Router
  , module           Nameservice.Modules.Nameservice.Types


  ) where

import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Query
import           Nameservice.Modules.Nameservice.Router
import           Nameservice.Modules.Nameservice.Store    (Name (..))
import           Nameservice.Modules.Nameservice.Types
import           Polysemy                                 (Members)
import           Tendermint.SDK.Application               (Module (..),
                                                           ModuleEffs)
import           Tendermint.SDK.BaseApp                   (DefaultCheckTx (..))
import           Tendermint.SDK.Modules.Bank              (Bank)


type Nameservice =
  Module NameserviceName MessageApi MessageApi QueryApi NameserviceEffs '[Bank]

nameserviceModule
  :: Members (ModuleEffs Nameservice) r
  => Nameservice r
nameserviceModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
