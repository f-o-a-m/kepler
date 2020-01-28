module Tendermint.SDK.Modules.Bank.Query where

import           Control.Lens                       ((^.))
import qualified Data.ByteArray.Base64String        as Base64
import           Polysemy
import           Servant.API
import qualified Tendermint.SDK.BaseApp             as BaseApp
import           Tendermint.SDK.BaseApp.Query       (QueryArgs (..))
import qualified Tendermint.SDK.Modules.Auth        as Auth
import           Tendermint.SDK.Modules.Bank.Keeper (getCoinBalance)
import           Tendermint.SDK.Types.Address       (Address)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type GetAddressCoinBalance =
     "balance"
  :> BaseApp.QA Address
  :> QueryParam' '[Required, Strict] "coin_id" Auth.CoinId
  :> BaseApp.Leaf Auth.Coin

getAddressCoinBalance
  :: Members Auth.AuthEffs r
  => QueryArgs Address
  -> Auth.CoinId
  -> Sem r (BaseApp.QueryResult Auth.Coin)
getAddressCoinBalance (QueryArgs _ address _) cid = do
  coin <- getCoinBalance address cid
  pure $ BaseApp.QueryResult
    { queryResultData = coin
    , queryResultIndex = 0
    , queryResultKey = Base64.fromBytes $ address ^. BaseApp.rawKey
    , queryResultProof  = Nothing
    , queryResultHeight = 0
    }

type QueryApi = GetAddressCoinBalance

server
  :: forall r.
     Members Auth.AuthEffs r
  => BaseApp.RouteQ QueryApi r
server = getAddressCoinBalance
