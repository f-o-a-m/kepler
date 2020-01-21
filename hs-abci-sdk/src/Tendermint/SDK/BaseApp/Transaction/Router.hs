module Tendermint.SDK.BaseApp.Transaction.Router where

import qualified Data.ByteArray.Base64String      as Base64
import           Data.Proxy
import           Data.String.Conversions          (cs)
import           GHC.TypeLits                     (KnownSymbol, symbolVal)
import           Polysemy                         (Sem)
import           Servant.API
import qualified Tendermint.SDK.BaseApp.Router    as R
import           Tendermint.SDK.Types.Transaction (PreRoutedTx)
import           Tendermint.SDK.Types.TxResult    (TxResult)

class HasRouter layout r where
  type RouteT layout r :: *
  route :: Proxy layout
        -> Proxy r
        -> R.Delayed (Sem r) env (PreRoutedTx Base64.Base64String) (RouteT layout r)
        -> R.Router env r (PreRoutedTx Base64.Base64String) TxResult

instance (HasRouter a r, HasRouter b r) => HasRouter (a :<|> b) r where
  type RouteT (a :<|> b) r = RouteT a r :<|> RouteT b r

  route _ pr server = R.choice (route pa pr ((\ (a :<|> _) -> a) <$> server))
                        (route pb pr ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance (HasRouter sublayout r, KnownSymbol path) => HasRouter (path :> sublayout) r where

  type RouteT (path :> sublayout) r = RouteT sublayout r

  route _ pr subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (route (Proxy :: Proxy sublayout) pr subserver)
    where proxyPath = Proxy :: Proxy path
