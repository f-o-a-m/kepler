module Nameservice.Modules.Nameservice.Query where

import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper (storeKey)
import           Nameservice.Modules.Nameservice.Types  (Name, Whois)
import           Polysemy                               (Member, Sem)
import           Servant.API                            ((:>))
import qualified Tendermint.SDK.Query                   as Q
import qualified Tendermint.SDK.Store                   as Store

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type NameserviceContents = '[(Name, Whois)]

type Api = "nameservice" :> Q.QueryApi NameserviceContents

server :: Member Store.RawStore r => Q.RouteT Api (Sem r)
server =
  Q.storeQueryHandlers (Proxy :: Proxy NameserviceContents) storeKey (Proxy :: Proxy (Sem r))
