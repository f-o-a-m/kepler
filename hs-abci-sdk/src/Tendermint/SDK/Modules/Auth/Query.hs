module Tendermint.SDK.Modules.Auth.Query
  ( Api
  , querier
  ) where

import           Polysemy                           (Members)
import           Servant.API                        ((:>))
import qualified Tendermint.SDK.BaseApp             as BaseApp
import           Tendermint.SDK.BaseApp.Query       (QueryEffs, StoreLeaf)
import qualified Tendermint.SDK.BaseApp.Store.Map   as M
import           Tendermint.SDK.Modules.Auth.Keeper (accountsMap)
import           Tendermint.SDK.Modules.Auth.Types  (Account)
import           Tendermint.SDK.Types.Address       (Address)

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type Api = "accounts" :> StoreLeaf (M.Map Address Account)

querier
  :: Members QueryEffs r
  => BaseApp.RouteQ Api r
querier =
  BaseApp.storeQueryHandler accountsMap
