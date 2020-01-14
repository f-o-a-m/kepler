module Tendermint.SDK.Modules.Bank.Query where

import           Data.Proxy
import           Polysemy
import           Polysemy.Error                     (Error)
import qualified Tendermint.SDK.BaseApp             as BaseApp
import           Tendermint.SDK.Modules.Bank.Keeper (storeKey)
import           Tendermint.SDK.Modules.Bank.Types  (Amount)
import           Tendermint.SDK.Types.Address       (Address)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type BankContents = '[(Address, Amount)]

type Api = BaseApp.QueryApi BankContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteT Api (Sem r)
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy BankContents) storeKey (Proxy :: Proxy (Sem r))
