module Tendermint.SDK.Modules.Token.Query where

import           Data.Proxy
import           Polysemy
import           Polysemy.Error                      (Error)
import qualified Tendermint.SDK.BaseApp              as BaseApp
import           Tendermint.SDK.Modules.Token.Keeper (storeKey)
import           Tendermint.SDK.Modules.Token.Types  (Amount)
import           Tendermint.SDK.Types.Address        (Address)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type TokenContents = '[(Address, Amount)]

type Api = BaseApp.QueryApi TokenContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteT Api (Sem r)
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy TokenContents) storeKey (Proxy :: Proxy (Sem r))
