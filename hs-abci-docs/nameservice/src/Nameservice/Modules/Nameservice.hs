module Nameservice.Modules.Nameservice

  (
    -- * Module
    NameserviceM
  , nameserviceModule
  , module           Nameservice.Modules.Nameservice.Keeper
  , module           Nameservice.Modules.Nameservice.Messages
  , module           Nameservice.Modules.Nameservice.Query
  , module           Nameservice.Modules.Nameservice.Router
  , module           Nameservice.Modules.Nameservice.Types


  ) where

import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper   hiding (storeKey)
import           Nameservice.Modules.Nameservice.Messages
import           Nameservice.Modules.Nameservice.Query
import           Nameservice.Modules.Nameservice.Router
import           Nameservice.Modules.Nameservice.Types
import           Tendermint.SDK.Application               (Module (..),
                                                           ModuleMembers)
import           Tendermint.SDK.BaseApp                   (DefaultCheckTx (..))
import           Tendermint.SDK.Modules.Bank              (BankM)


type NameserviceM =
  Module "nameservice" MessageApi MessageApi QueryApi NameserviceEffs '[BankM]

nameserviceModule
  :: ModuleMembers NameserviceM r
  => NameserviceM r
nameserviceModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
