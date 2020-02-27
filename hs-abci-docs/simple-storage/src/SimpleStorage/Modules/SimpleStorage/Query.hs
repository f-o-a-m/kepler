module SimpleStorage.Modules.SimpleStorage.Query
  ( CountStoreContents
  , QueryApi
  , querier
  ) where

import           Data.Proxy
import           Polysemy                                   (Members)
import           SimpleStorage.Modules.SimpleStorage.Keeper (store)
import           SimpleStorage.Modules.SimpleStorage.Types  (Count, CountKey)
import qualified Tendermint.SDK.BaseApp                     as BaseApp


type CountStoreContents = '[(CountKey, Count)]

type QueryApi = BaseApp.QueryApi CountStoreContents

querier
  :: Members BaseApp.QueryEffs r
  => BaseApp.RouteQ QueryApi r
querier =
  BaseApp.storeQueryHandlers (Proxy :: Proxy CountStoreContents)
    store (Proxy :: Proxy r)
