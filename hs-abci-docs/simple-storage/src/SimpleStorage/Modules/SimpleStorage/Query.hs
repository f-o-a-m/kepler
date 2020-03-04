module SimpleStorage.Modules.SimpleStorage.Query
  ( QueryApi
  , querier
  ) where

import           Polysemy                                   (Members)
import           Servant.API                                ((:>))
import           SimpleStorage.Modules.SimpleStorage.Keeper (countVar)
import           SimpleStorage.Modules.SimpleStorage.Types  (Count)
import qualified Tendermint.SDK.BaseApp                     as BaseApp
import qualified Tendermint.SDK.BaseApp.Store.Var           as V


type QueryApi = "count" :> BaseApp.StoreLeaf (V.Var Count)

querier
  :: Members BaseApp.QueryEffs r
  => BaseApp.RouteQ QueryApi r
querier = BaseApp.storeQueryHandler countVar
