module Tendermint.SDK.BaseApp.BaseApp
  ( BaseAppEffs
  , compileToCoreEffs
  ) where

import           Control.Exception                         (throwIO)
import           Control.Monad.IO.Class                    (liftIO)
import           Polysemy                                  (Sem)
import           Polysemy.Error                            (Error, runError)
--import           Polysemy.Reader                           (ask)
import           Polysemy.Resource                         (Resource,
                                                            resourceToIO)
import           Tendermint.SDK.BaseApp.CoreEff            (CoreEffs)
import           Tendermint.SDK.BaseApp.Errors             (AppError)
--                                                            makeAppError)
import           Tendermint.SDK.BaseApp.Logger             (Logger)
import qualified Tendermint.SDK.BaseApp.Logger.Katip       as KL
import           Tendermint.SDK.BaseApp.Metrics            (Metrics)
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus as Prometheus
--import           Tendermint.SDK.BaseApp.Store              (ReadStore)
--import qualified Tendermint.SDK.BaseApp.Store.IAVLStore    as IAVL
import           Tendermint.SDK.Types.Effects              ((:&))

-- | Concrete row of effects for the BaseApp. Note that because there does
-- | not exist an interpreter for an untagged 'RawStore', you must scope
-- | these effects before they can be interpreted.
type BaseAppEffs =
  [ Metrics
  , Logger
  , Resource
  , Error AppError
  ]

-- | An intermediary interpeter, bringing 'BaseApp' down to 'CoreEff'.
compileToCoreEffs
  :: forall a.
     Sem (BaseAppEffs :& CoreEffs) a
  -> Sem CoreEffs a
compileToCoreEffs action = do
  --grpc <- ask @IAVL.GrpcClient
  --version  <- do
  --  IAVL.IAVLVersions{..} <- ask @IAVL.IAVLVersions
  --  pure $ case scope of
  --    Consensus       -> latest
  --    QueryAndMempool -> committed
  eRes <- runError .
    resourceToIO .
    KL.evalKatip .
    Prometheus.evalWithMetrics $
   -- IAVL.evalRead grpc version $
    action
  either (liftIO . throwIO) return eRes
