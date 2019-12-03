module Nameservice.Modules.Token.Query where

import           Data.Proxy
import           Nameservice.Modules.Token.Keeper (storeKey)
import           Nameservice.Modules.Token.Types  (Amount)
import           Polysemy
import           Polysemy.Error                   (Error)
import           Polysemy.Tagged                  (Tagged)
import           Servant.API                      ((:>))
import           Tendermint.SDK.Errors            (AppError)
import           Tendermint.SDK.Query             (RouteT)
import           Tendermint.SDK.Query.Store       (QueryApi, storeQueryHandlers)
import qualified Tendermint.SDK.Store             as Store
import           Tendermint.SDK.Types.Address     (Address)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type TokenContents = '[(Address, Amount)]

type Api = "token" :> QueryApi TokenContents

server
  :: Members [Tagged 'Store.Query Store.RawStore, Error AppError] r
  => RouteT Api (Sem r)
server =
  storeQueryHandlers (Proxy :: Proxy TokenContents) storeKey (Proxy :: Proxy (Sem r))
