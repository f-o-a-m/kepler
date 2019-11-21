module Nameservice.Modules.Token.Query where

import           Tendermint.SDK.Types.Address    (Address)
import           Tendermint.SDK.Query.Store      (QueryApi, storeQueryHandlers)
import           Data.Proxy
import           Nameservice.Modules.Token.Types (Amount)
import           Servant.API                     ((:>))
import           Polysemy
import qualified Tendermint.SDK.Store            as Store
import           Tendermint.SDK.Query            (RouteT)
import Nameservice.Modules.Token.Keeper (storeKey)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type TokenContents = '[(Address, Amount)]

type Api = "token" :> QueryApi TokenContents

server :: Member Store.RawStore r => RouteT Api (Sem r)
server =
  storeQueryHandlers (Proxy :: Proxy TokenContents) storeKey (Proxy :: Proxy (Sem r))
