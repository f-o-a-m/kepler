{-# LANGUAGE UndecidableInstances #-}
module Tendermint.SDK.BaseApp.Transaction.Router
  ( HasTxRouter(..)
  , emptyTxServer
  ) where

import           Control.Monad.IO.Class                      (liftIO)
import           Data.ByteString                             (ByteString)
import           Data.Proxy
import           Data.String.Conversions                     (cs)
import           GHC.TypeLits                                (KnownSymbol,
                                                              symbolVal)
import           Polysemy                                    (Embed, Members,
                                                              Sem)
import           Servant.API
import qualified Tendermint.SDK.BaseApp.Router               as R
import           Tendermint.SDK.BaseApp.Transaction.Effect   (TxEffs, eval, newTransactionContext)
import           Tendermint.SDK.BaseApp.Transaction.Modifier
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Codec                        (HasCodec (..))
import           Tendermint.SDK.Types.Effects                ((:&))
import           Tendermint.SDK.BaseApp.Store (RawStore)
import           Tendermint.SDK.Types.Message                (HasMessageType (..),
                                                              Msg (..))
import           Tendermint.SDK.Types.TxResult               (TxResult)

--------------------------------------------------------------------------------

class HasTxRouter layout r (c :: RouteContext) where
  type RouteTx layout r c :: *
  routeTx
        :: Proxy layout
        -> Proxy r
        -> Proxy c
        -> R.Delayed (Sem r) env (RoutingTx ByteString) (RouteTx layout r c)
        -> R.Router env r (RoutingTx ByteString) TxResult

instance (HasTxRouter a r c, HasTxRouter b r c) => HasTxRouter (a :<|> b) r c where
  type RouteTx (a :<|> b) r c = RouteTx a r c :<|> RouteTx b r c

  routeTx _ pr pc server =
    R.choice (routeTx pa pr pc ((\ (a :<|> _) -> a) <$> server))
             (routeTx pb pr pc ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance (HasTxRouter sublayout r c, KnownSymbol path) => HasTxRouter (path :> sublayout) r c where

  type RouteTx (path :> sublayout) r c = RouteTx sublayout r c

  routeTx _ pr pc subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (routeTx (Proxy :: Proxy sublayout) pr pc subserver)
    where proxyPath = Proxy :: Proxy path

methodRouter
  :: HasCodec a
  => Members [Embed IO, RawStore] r
  => R.Delayed (Sem r) env (RoutingTx msg) (Sem (TxEffs :& r) a)
  -> R.Router env r (RoutingTx msg) TxResult
methodRouter action = R.leafRouter route'
  where
    route' env tx = do
      ctx <- liftIO $ newTransactionContext tx
      let action' = eval ctx <$> action
      R.runAction action' env tx R.Route

instance ( HasMessageType msg, HasCodec msg, HasCodec (OnCheckReturn c oc a), Members [RawStore, Embed IO] r) => HasTxRouter (TypedMessage msg :~> Return' oc a) r c where

  type RouteTx (TypedMessage msg :~> Return' oc a) r c = RoutingTx msg -> Sem (TxEffs :& r) (OnCheckReturn c oc a)

  routeTx _ _ _ subserver =
    let f (RoutingTx tx@Tx{txMsg}) =
          if msgType txMsg == mt
            then case decode $ msgData txMsg of
              Left e -> R.delayedFail $
                R.InvalidRequest ("Failed to parse message of type " <> mt <> ": " <> e <> ".")
              Right a -> pure . RoutingTx $ tx {txMsg = txMsg {msgData = a}}
            else R.delayedFail R.PathNotFound
    in methodRouter $
         R.addBody subserver $ R.withRequest f
      where mt = messageType (Proxy :: Proxy msg)

emptyTxServer :: RouteTx EmptyTxServer r c
emptyTxServer = EmptyTxServer

instance HasTxRouter EmptyTxServer r c where
  type RouteTx EmptyTxServer r c = EmptyTxServer
  routeTx _ _ _ _ = R.StaticRouter mempty mempty
