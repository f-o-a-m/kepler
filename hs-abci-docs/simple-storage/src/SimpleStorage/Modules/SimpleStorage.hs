module SimpleStorage.Modules.SimpleStorage
  ( SimpleStorage
  , simpleStorageModule

  , module SimpleStorage.Modules.SimpleStorage.Keeper
  , module SimpleStorage.Modules.SimpleStorage.Message
  , module SimpleStorage.Modules.SimpleStorage.Types
  ) where

import           Data.Proxy
import           Polysemy                                    (Members)
import           SimpleStorage.Modules.SimpleStorage.Keeper  hiding (storeKey)
import           SimpleStorage.Modules.SimpleStorage.Message
import           SimpleStorage.Modules.SimpleStorage.Query
import           SimpleStorage.Modules.SimpleStorage.Router
import           SimpleStorage.Modules.SimpleStorage.Types
import           Tendermint.SDK.Application                  (ComponentEffs,
                                                              Module (..))
import qualified Tendermint.SDK.BaseApp                      as BaseApp

type SimpleStorage =
  Module SimpleStorageName MessageApi MessageApi QueryApi SimpleStorageEffs '[]

simpleStorageModule
  :: Members (ComponentEffs SimpleStorage) r
  => SimpleStorage r
simpleStorageModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = BaseApp.defaultCheckTx (Proxy @MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
