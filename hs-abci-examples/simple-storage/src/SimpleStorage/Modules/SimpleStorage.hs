module SimpleStorage.Modules.SimpleStorage
  ( SimpleStorageM
  , Api
  , simpleStorageModule
  , module SimpleStorage.Modules.SimpleStorage.Keeper
  , module SimpleStorage.Modules.SimpleStorage.Message
  , module SimpleStorage.Modules.SimpleStorage.Types
  ) where

import           Polysemy                                    (Member, Members)
import           SimpleStorage.Modules.SimpleStorage.Keeper  hiding (storeKey)
import           SimpleStorage.Modules.SimpleStorage.Message
import           SimpleStorage.Modules.SimpleStorage.Query   (Api, server)
import           SimpleStorage.Modules.SimpleStorage.Router  (router)
import           SimpleStorage.Modules.SimpleStorage.Types
import           Tendermint.SDK.Application                  (Module (..),
                                                              defaultTxChecker)
import qualified Tendermint.SDK.BaseApp                      as BaseApp

type SimpleStorageM r =
  Module "simple_storage" SimpleStorageMessage () Api SimpleStorageEffs r

simpleStorageModule
  :: Member SimpleStorage r
  => Members BaseApp.BaseAppEffs r
  => SimpleStorageM r
simpleStorageModule = Module
  { moduleTxDeliverer = router
  , moduleTxChecker = defaultTxChecker
  , moduleQueryServer = server
  , moduleEval = eval
  }
