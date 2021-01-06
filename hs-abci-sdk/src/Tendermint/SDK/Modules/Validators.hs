module Tendermint.SDK.Modules.Validators
  (
    Validators
  , validatorsModule

  , module Tendermint.SDK.Modules.Validators.Keeper
  , module Tendermint.SDK.Modules.Validators.Types

  , endBlock
  ) where


import           Polysemy                                   (Members)
import           Tendermint.SDK.Application                 (Module (..),
                                                             ModuleEffs)
import           Tendermint.SDK.BaseApp                     (EmptyTxServer (..))
import           Tendermint.SDK.Modules.Validators.EndBlock
import           Tendermint.SDK.Modules.Validators.Keeper
import           Tendermint.SDK.Modules.Validators.Query
import           Tendermint.SDK.Modules.Validators.Types


type Validators = Module ValidatorsName EmptyTxServer EmptyTxServer QueryApi ValidatorsEffs '[]

validatorsModule ::
  Members (ModuleEffs Validators) r =>
  Validators r
validatorsModule =
  Module
    { moduleTxDeliverer = EmptyTxServer,
      moduleTxChecker = EmptyTxServer,
      moduleQuerier = querier,
      moduleEval = eval
    }

