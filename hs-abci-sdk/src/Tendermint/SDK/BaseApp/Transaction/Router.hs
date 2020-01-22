{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Transaction.Router where

import           Control.Monad.IO.Class             (liftIO)
import qualified Data.ByteArray.Base64String        as Base64
import           Data.Proxy
import           Data.String.Conversions            (cs)
import           GHC.TypeLits                       (KnownSymbol, Symbol,
                                                     symbolVal)
import           Polysemy                           (Embed, Member, Sem)
import           Servant.API
import qualified Tendermint.SDK.BaseApp.Router      as R
import           Tendermint.SDK.BaseApp.Transaction (TxEffs, eval,
                                                     newTransactionContext)
import           Tendermint.SDK.Codec               (HasCodec (..))
import           Tendermint.SDK.Types.Effects       ((:&))
import           Tendermint.SDK.Types.Message       (Msg (..))
import           Tendermint.SDK.Types.Transaction   (PreRoutedTx (..), Tx (..))
import           Tendermint.SDK.Types.TxResult      (TxResult)

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

data TypedMessage (t :: Symbol) a

instance (HasRouter sublayout r, KnownSymbol t, HasCodec msg) => HasRouter (TypedMessage t msg :> sublayout) r where

  type RouteT (TypedMessage t msg :> sublayout) r = PreRoutedTx msg -> RouteT sublayout r

  route _ pr subserver =
    let f (PreRoutedTx tx@Tx{txMsg}) =
          if msgType txMsg == messageType
            then case decode . Base64.toBytes $ msgData txMsg of
              Left e -> R.delayedFail $
                R.InvalidRequest ("Failed to parse message of type " <> messageType <> ": " <> e <> ".")
              Right a -> pure . PreRoutedTx $ tx {txMsg = txMsg {msgData = a}}
            else R.delayedFail R.PathNotFound
    in route (Proxy :: Proxy sublayout) pr $
         R.addBody subserver $ R.withRequest f
      where messageType = cs $ symbolVal (Proxy :: Proxy t)

data Result a

instance (Member (Embed IO) r, HasCodec a) => HasRouter (Result a) r where

  type RouteT (Result a) r = Sem (TxEffs :& r) a

  route _ _ = methodRouter

methodRouter
  :: HasCodec a
  => Member (Embed IO) r
  => R.Delayed (Sem r) env (PreRoutedTx msg) (Sem (TxEffs :& r) a)
  -> R.Router env r (PreRoutedTx msg) TxResult
methodRouter action = R.leafRouter route'
  where
    route' env tx = do
      ctx <- liftIO $ newTransactionContext tx
      let action' = eval ctx <$> action
      R.runAction action' env tx R.Route
