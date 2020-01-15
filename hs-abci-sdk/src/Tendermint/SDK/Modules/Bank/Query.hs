module Tendermint.SDK.Modules.Bank.Query where

import           Data.Proxy
import           Polysemy
import           Polysemy.Error                     (Error)
import           Servant.API
import qualified Tendermint.SDK.BaseApp             as BaseApp
import           Tendermint.SDK.BaseApp.Query.Types (Leaf (..), QueryArgs (..))
import qualified Tendermint.SDK.Modules.Auth        as Auth
import           Tendermint.SDK.Types.Address       (Address)
import Data.Text (Text)

--------------------------------------------------------------------------------
-- | Query Api
--------------------------------------------------------------------------------

type Api = "balance" :> QueryArgs (Address, Text) :> Leaf Auth.Coin

server
  :: Members Auth.AuthEffs r
  => Member (Error BaseApp.AppError) r
  => BaseApp.RouteT Api (Sem r)
server =
  undefined
