module SimpleStorage.Modules.SimpleStorage.Query
  ( CountStoreContents
  , QueryApi
  , server
  ) where

import           Data.Proxy
import           Polysemy                                   (Members)
import           Polysemy.Error                             (Error)
import           SimpleStorage.Modules.SimpleStorage.Keeper (storeKey)
import           SimpleStorage.Modules.SimpleStorage.Types  (Count, CountKey)
import qualified Tendermint.SDK.BaseApp                     as BaseApp


type CountStoreContents = '[(CountKey, Count)]

type QueryApi = BaseApp.QueryApi CountStoreContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteQ QueryApi r
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy CountStoreContents)
    storeKey (Proxy :: Proxy r)
