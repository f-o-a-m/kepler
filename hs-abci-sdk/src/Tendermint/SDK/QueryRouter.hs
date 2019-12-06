module Tendermint.SDK.QueryRouter
  ( router
  , QueryRouter(..)
  ) where

import           Data.Proxy
import           Polysemy              (Sem)
import           Servant.API           ((:<|>) (..))
import           Tendermint.SDK.Module (Module (..), Modules (..))
import           Tendermint.SDK.Query  (HasRouter, QueryApplication, RouteT,
                                        serve)

router
  :: QueryRouter ms r
  => HasRouter (Api ms)
  => Modules ms r
  -> QueryApplication (Sem r)
router (ms :: Modules ms r) = serve (Proxy :: Proxy (Api ms)) (route ms)

class QueryRouter ms r where
    type Api ms :: *
    route :: Modules ms r -> RouteT (Api ms) (Sem r)

instance QueryRouter (Module name msg api r ': '[]) r where
    type Api (Module name msg api r ': '[]) = api
    route (ConsModule m NilModules) = moduleQueryServer m

instance QueryRouter (m' ': ms) r => QueryRouter (Module name msg api r ': m' ': ms) r where
    type Api (Module name msg api r ': m' ': ms) = api :<|> Api (m' ': ms)
    route (ConsModule m rest) = moduleQueryServer m :<|> route rest
