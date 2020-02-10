module Tendermint.SDK.Modules.Auth.Query
  ( Api
  , querier
  ) where

import           Data.Proxy
import           Polysemy                           (Members)
import qualified Tendermint.SDK.BaseApp             as BaseApp
import           Tendermint.SDK.BaseApp.Query       (QueryEffs)
import           Tendermint.SDK.Modules.Auth.Keeper (storeKey)
import           Tendermint.SDK.Modules.Auth.Types  (Account)
import           Tendermint.SDK.Types.Address       (Address)

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type AuthContents = '[(Address, Account)]

type Api = BaseApp.QueryApi AuthContents

querier
  :: Members QueryEffs r
  => BaseApp.RouteQ Api r
querier =
  BaseApp.storeQueryHandlers (Proxy :: Proxy AuthContents) storeKey (Proxy :: Proxy r)
