module SimpleStorage.Modules.SimpleStorage.Query
  ( CountStoreContents
  , Api
  , server
  ) where

import           Data.Proxy
import           Polysemy                                   (Members, Sem)
import           Polysemy.Error                             (Error)
import           SimpleStorage.Modules.SimpleStorage.Keeper (storeKey)
import           SimpleStorage.Modules.SimpleStorage.Types  (Count, CountKey)
import qualified Tendermint.SDK.BaseApp                     as BaseApp


type CountStoreContents = '[(CountKey, Count)]

type Api = BaseApp.QueryApi CountStoreContents

server
  :: Members [BaseApp.RawStore, Error BaseApp.AppError] r
  => BaseApp.RouteT Api (Sem r)
server =
  BaseApp.storeQueryHandlers (Proxy :: Proxy CountStoreContents)
    storeKey (Proxy :: Proxy (Sem r))
