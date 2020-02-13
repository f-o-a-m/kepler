module Tendermint.SDK.BaseApp.BaseEffs
  ( BaseEffs
  , compileToCore
  ) where

import           Control.Exception                         (throwIO)
import           Control.Monad.IO.Class                    (liftIO)
import           Polysemy                                  (Embed, Members, Sem)
import           Polysemy.Error                            (Error, runError)
import           Polysemy.Reader                           (Reader)
import           Polysemy.Resource                         (Resource,
                                                            resourceToIO)
import           Tendermint.SDK.BaseApp.Errors             (AppError)
import           Tendermint.SDK.BaseApp.Logger             (Logger)
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import           Tendermint.SDK.BaseApp.Metrics            (Metrics)
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus as Prometheus
import           Tendermint.SDK.Types.Effects              ((:&))

-- | Concrete row of effects for the BaseApp. Note that because there does
-- | not exist an interpreter for an untagged 'RawStore', you must scope
-- | these effects before they can be interpreted.
type BaseEffs =
  [ Metrics
  , Logger
  , Resource
  , Error AppError
  ]

-- | An intermediary interpeter, bringing 'BaseApp' down to 'CoreEff'.
compileToCore
  :: Members [Embed IO, Reader KL.LogConfig, Reader (Maybe Prometheus.PrometheusEnv)] core
  => forall a.
     Sem (BaseEffs :& core) a
  -> Sem core a
compileToCore action = do
  eRes <- runError .
    resourceToIO .
    KL.evalKatip .
    Prometheus.evalWithMetrics $
    action
  either (liftIO . throwIO) return eRes
