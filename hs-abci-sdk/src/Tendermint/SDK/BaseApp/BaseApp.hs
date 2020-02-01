module Tendermint.SDK.BaseApp.BaseApp
  ( BaseAppEffs
  , compileToCoreEffs
  ) where

import           Control.Exception                         (throwIO)
import           Control.Monad.IO.Class                    (liftIO)
import           Polysemy                                  (Sem)
import           Polysemy.Error                            (Error, runError)
import           Polysemy.Resource                         (Resource,
                                                            resourceToIO)
import           Tendermint.SDK.BaseApp.CoreEff            (CoreEffs)
import           Tendermint.SDK.BaseApp.Errors             (SDKError,
                                                            makeAppError)
import           Tendermint.SDK.BaseApp.Logger             (Logger)
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import           Tendermint.SDK.BaseApp.Metrics            (Metrics)
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus as Prometheus
import           Tendermint.SDK.Types.Effects              ((:&))

-- | Concrete row of effects for the BaseApp. Note that because there does
-- | not exist an interpreter for an untagged 'RawStore', you must scope
-- | these effects before they can be interpreted.
type BaseAppEffs =
  [ Metrics
  , Logger
  , Resource
  , Error SDKError
  ]

-- | An intermediary interpeter, bringing 'BaseApp' down to 'CoreEff'.
compileToCoreEffs
  :: forall a.
     Sem (BaseAppEffs :& CoreEffs) a
  -> Sem CoreEffs a
compileToCoreEffs action = do
  eRes <- runError .
    resourceToIO .
    KL.evalKatip .
    Prometheus.evalWithMetrics $ action
  either (liftIO . throwIO . makeAppError) return eRes
