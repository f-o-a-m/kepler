module Tendermint.SDK.Modules.Auth.Query where

import           Data.Proxy
import           Polysemy                           (Members, Sem)
import           Polysemy.Error                     (Error)
import qualified Tendermint.SDK.BaseApp             as BaseApp
import           Tendermint.SDK.Modules.Auth.Keeper (storeKey)
import           Tendermint.SDK.Modules.Auth.Types  (Account)
import           Tendermint.SDK.Types.Address       (Address)

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type AuthContents = '[(Address, Account)]

type Api = BaseApp.QueryApi AuthContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteT Api (Sem r)
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy AuthContents) storeKey (Proxy :: Proxy (Sem r))
