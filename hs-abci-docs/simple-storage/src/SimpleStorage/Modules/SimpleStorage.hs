module SimpleStorage.Modules.SimpleStorage
  ( SimpleStorageM
  , QueryApi
  , MessageApi
  , simpleStorageModule
  , module SimpleStorage.Modules.SimpleStorage.Keeper
  , module SimpleStorage.Modules.SimpleStorage.Message
  , module SimpleStorage.Modules.SimpleStorage.Types
  ) where

import           Data.Proxy
import           Polysemy                                    (Member, Members)
import           SimpleStorage.Modules.SimpleStorage.Keeper  hiding (storeKey)
import           SimpleStorage.Modules.SimpleStorage.Message
import           SimpleStorage.Modules.SimpleStorage.Query   (QueryApi, querier)
import           SimpleStorage.Modules.SimpleStorage.Router  (MessageApi,
                                                              messageHandlers)
import           SimpleStorage.Modules.SimpleStorage.Types
import           Tendermint.SDK.Application                  (Module (..))
import qualified Tendermint.SDK.BaseApp                      as BaseApp

type SimpleStorageM =
  Module "simple_storage" MessageApi MessageApi QueryApi SimpleStorageEffs '[]

simpleStorageModule
  :: Member SimpleStorage r
  => Members BaseApp.TxEffs r
  => Members BaseApp.BaseEffs r
  => SimpleStorageM r
simpleStorageModule = Module
  { moduleTxDeliverer = messageHandlers
  , moduleTxChecker = BaseApp.defaultCheckTx (Proxy @MessageApi) (Proxy :: Proxy r)
  , moduleQuerier = querier
  , moduleEval = eval
  }
