module Nameservice.Modules.Nameservice.Query where

import           Data.Proxy
import           Nameservice.Modules.Nameservice.Keeper (storeKey)
import           Nameservice.Modules.Nameservice.Types  (Name, Whois)
import           Polysemy                               (Members, Sem)
import           Polysemy.Error                         (Error)
import           Servant.API                            ((:>))
import           Tendermint.SDK.Errors                  (AppError)
import qualified Tendermint.SDK.Query                   as Q
import qualified Tendermint.SDK.Store                   as Store

--------------------------------------------------------------------------------
-- | Query API
--------------------------------------------------------------------------------

type NameserviceContents = '[(Name, Whois)]

type Api = "nameservice" :> Q.QueryApi NameserviceContents

server
  :: Members [Store.RawStore, Error AppError] r
  => Q.RouteT Api (Sem r)
server =
  Q.storeQueryHandlers (Proxy :: Proxy NameserviceContents) storeKey (Proxy :: Proxy (Sem r))
