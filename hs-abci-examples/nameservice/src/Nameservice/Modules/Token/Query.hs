module Nameservice.Modules.Token.Query where

import           Data.Proxy
import           Nameservice.Modules.Token.Keeper (storeKey)
import           Nameservice.Modules.Token.Types  (Amount)
import           Polysemy
import           Polysemy.Error                   (Error)
import qualified Tendermint.SDK.BaseApp           as BaseApp
import           Tendermint.SDK.Types.Address     (Address)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type TokenContents = '[(Address, Amount)]

type Api = BaseApp.QueryApi TokenContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteT Api r
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy TokenContents) storeKey (Proxy :: Proxy r)
