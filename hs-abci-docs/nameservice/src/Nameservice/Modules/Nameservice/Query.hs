module Nameservice.Modules.Nameservice.Query
  ( QueryApi
  , querier
  ) where

import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper (store)
import           Nameservice.Modules.Nameservice.Types  (Name, Whois)
import           Polysemy                               (Members)
import qualified Tendermint.SDK.BaseApp                 as BaseApp

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type NameserviceContents = '[(Name, Whois)]

type QueryApi = BaseApp.QueryApi NameserviceContents

querier
  :: Members BaseApp.QueryEffs r
  => BaseApp.RouteQ QueryApi r
querier =
  BaseApp.storeQueryHandlers (Proxy :: Proxy NameserviceContents) store (Proxy :: Proxy r)
