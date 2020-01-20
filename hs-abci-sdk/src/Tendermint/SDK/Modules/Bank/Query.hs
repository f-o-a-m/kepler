module Tendermint.SDK.Modules.Bank.Query where

import           Polysemy
import           Servant.API
import qualified Tendermint.SDK.BaseApp             as BaseApp
import qualified Tendermint.SDK.Modules.Auth        as Auth
import           Tendermint.SDK.Modules.Bank.Keeper (BankEffs, getBalance)
import           Tendermint.SDK.Types.Address       (Address)
--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type GetAddressCoinBalance =
     "balance"
  :> QueryParam' '[Required, Strict] "address" Address
  :> QueryParam' '[Required, Strict] "coin_id" Auth.CoinId
  :> BaseApp.Leaf Auth.Coin

getAddressCoinBalance
  :: Members BankEffs r
  => Address
  -> Auth.CoinId
  -> Sem r (BaseApp.QueryResult Auth.Coin)
getAddressCoinBalance address cid = do
  coin <- getBalance address cid
  pure $ BaseApp.QueryResult
    { queryResultData = coin
    , queryResultIndex = 0
    , queryResultKey = undefined
    , queryResultProof  = Nothing
    , queryResultHeight = 0
    }

type Api = GetAddressCoinBalance

server
  :: forall r.
     Members BankEffs r
  => BaseApp.RouteT Api r
server = getAddressCoinBalance
