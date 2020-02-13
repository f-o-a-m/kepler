module Nameservice.Modules.Token.Query where

import           Data.Proxy
import           Nameservice.Modules.Token.Keeper (storeKey)
import           Nameservice.Modules.Token.Types  (Amount)
import           Polysemy
import qualified Tendermint.SDK.BaseApp           as BaseApp
import           Tendermint.SDK.Types.Address     (Address)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type TokenContents = '[(Address, Amount)]

type QueryApi = BaseApp.QueryApi TokenContents

querier
  :: Members BaseApp.QueryEffs r
  => BaseApp.RouteQ QueryApi r
querier =
  BaseApp.storeQueryHandlers (Proxy :: Proxy TokenContents) storeKey (Proxy :: Proxy r)
