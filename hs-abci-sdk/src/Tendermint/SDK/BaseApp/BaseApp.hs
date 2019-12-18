module Tendermint.SDK.BaseApp.BaseApp
  ( BaseAppEffs
  , BaseApp
  , ScopedBaseApp
  , ScopedEff(..)
  , compileScopedEff
  , compileToCoreEffs
  , (:&)
  ) where

import           Control.Exception                          (throwIO)
import           Control.Monad.IO.Class                     (liftIO)
import           Polysemy                                   (Sem)
import           Polysemy.Error                             (Error, runError)
import           Polysemy.Output                            (Output)
import           Polysemy.Resource                          (Resource,
                                                             resourceToIO)
import           Tendermint.SDK.BaseApp.CoreEff             (CoreEffs)
import           Tendermint.SDK.BaseApp.Errors              (AppError)
import           Tendermint.SDK.BaseApp.Events              (Event,
                                                             evalWithBuffer)
import           Tendermint.SDK.BaseApp.Logger              (Logger)
import qualified Tendermint.SDK.BaseApp.Logger.Katip        as KL
import           Tendermint.SDK.BaseApp.Metrics             (Metrics)
import qualified Tendermint.SDK.BaseApp.Metrics.Prometheus  as Prometheus
import           Tendermint.SDK.BaseApp.Store               (ApplyScope, ConnectionScope (..),
                                                             RawStore,
                                                             ResolveScope (..))
import qualified Tendermint.SDK.BaseApp.Store.AuthTreeStore as AT

-- | Concrete row of effects for the BaseApp. Note that because there does
-- | not exist an interpreter for an untagged 'RawStore', you must scope
-- | these effects before they can be interpreted.
type BaseAppEffs =
  [ RawStore
  , Output Event
  , Metrics
  , Logger
  , Resource
  , Error AppError
  ]

-- | This type family gives a nice syntax for combining multiple lists of effects.
type family (as :: [a]) :& (bs :: [a]) :: [a] where
  '[] :& bs = bs
  (a ': as) :& bs = a ': (as :& bs)

infixr 5 :&

type BaseApp r = BaseAppEffs :& r

type ScopedBaseApp (s :: ConnectionScope) r = ApplyScope s (BaseApp r)

-- | An intermediary interpeter, bringing 'BaseApp' down to 'CoreEff'.
compileToCoreEffs
  :: AT.AuthTreeGetter s
  => forall a. Sem (ScopedBaseApp s CoreEffs) a -> Sem CoreEffs a
compileToCoreEffs action = do
  eRes <- runError .
    resourceToIO .
    KL.evalKatip .
    Prometheus.evalWithMetrics .
    evalWithBuffer .
    resolveScope $ action
  either (liftIO . throwIO) return eRes

data ScopedEff r a where
  QueryScoped :: Sem (ScopedBaseApp 'Query r) a -> ScopedEff r a
  MempoolScoped :: Sem (ScopedBaseApp 'Mempool r) a -> ScopedEff r a
  ConsensusScoped :: Sem (ScopedBaseApp 'Consensus r) a -> ScopedEff r a

compileScopedEff :: ScopedEff CoreEffs a -> Sem CoreEffs a
compileScopedEff = \case
  QueryScoped m -> compileToCoreEffs m
  MempoolScoped m -> compileToCoreEffs m
  ConsensusScoped m -> compileToCoreEffs m
