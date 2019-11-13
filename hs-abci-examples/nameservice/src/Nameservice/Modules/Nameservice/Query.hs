module Nameservice.Modules.Nameservice.Query where

import           Data.Proxy
import           Nameservice.Modules.Nameservice.Types (Name, Whois)
import           Polysemy                              (Member, Sem)
import           Servant.API                           ((:>))
import qualified Tendermint.SDK.Router                 as R
import qualified Tendermint.SDK.Store                  as Store
import           Tendermint.SDK.StoreQueries           (QueryApi,
                                                        storeQueryHandlers)

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type NameserviceContents = '[(Name, Whois)]

type Api = "nameservice" :> QueryApi NameserviceContents

server :: Member Store.RawStore r => R.RouteT Api (Sem r)
server =
  storeQueryHandlers (Proxy :: Proxy NameserviceContents) (Proxy :: Proxy "nameservice") (Proxy :: Proxy (Sem r))
