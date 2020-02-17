module Nameservice.Modules.Nameservice

  (
    -- * Module
    Nameservice
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
import           Polysemy                                 (Members)
import           Tendermint.SDK.Application               (ComponentEffs,
                                                           Module (..))
import           Tendermint.SDK.BaseApp                   (DefaultCheckTx (..))
import           Tendermint.SDK.Modules.Bank              (Bank)


type Nameservice =
  Module "nameservice" MessageApi MessageApi QueryApi NameserviceEffs '[Bank]

nameserviceModule
  :: Members (ComponentEffs Nameservice) r
  => Nameservice r
nameserviceModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = defaultCheckTx (Proxy :: Proxy MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
