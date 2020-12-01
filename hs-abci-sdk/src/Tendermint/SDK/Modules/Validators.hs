module Tendermint.SDK.Modules.Validators where

import           Polysemy                                     (Members)
import           Tendermint.SDK.Application.Module            (Module (..),
                                                               ModuleEffs)
import           Tendermint.SDK.BaseApp                       (EmptyQueryServer (..),
                                                               EmptyTxServer (..))
import           Tendermint.SDK.Modules.Validators.BeginBlock
import           Tendermint.SDK.Modules.Validators.EndBlock
import           Tendermint.SDK.Modules.Validators.Keeper
import           Tendermint.SDK.Modules.Validators.Types

type Validators = Module ValidatorsName EmptyTxServer EmptyTxServer EmptyQueryServer BeginBlockAPI EndBlockAPI ValidatorsEffs '[]

validatorsModule ::
  Members (ModuleEffs Validators) r =>
  Validators r
validatorsModule =
  Module
    { moduleTxDeliverer = EmptyTxServer,
      moduleTxChecker = EmptyTxServer,
      moduleQuerier = EmptyQueryServer,
      moduleBeginBlocker = beginBlocker,
      moduleEndBlocker = endBlocker,
      moduleEval = eval
    }
