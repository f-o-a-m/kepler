module Tendermint.SDK.Modules.Bank.Query where

import           Data.Proxy
import           Polysemy
import           Polysemy.Error                     (Error)
import qualified Tendermint.SDK.BaseApp             as BaseApp
import qualified Tendermint.SDK.Modules.Auth        as Auth
import           Tendermint.SDK.Modules.Bank.Keeper (storeKey)
import           Tendermint.SDK.Types.Address       (Address)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------


type BankContents = '[(Address, Auth.Coin)]

type Api = BaseApp.QueryApi BankContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteT Api (Sem r)
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy BankContents) storeKey (Proxy :: Proxy (Sem r))

-- type Api = "balance" :> QueryArgs (Address, Text) :> Leaf Auth.Coin

-- server
--   :: --  Members Auth.AuthEffs r
--   -- => Member (Error BaseApp.AppError) r
--   -- =>
--   BaseApp.RouteT Api (Sem r)
-- server = undefined

