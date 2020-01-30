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
import           Tendermint.SDK.BaseApp.Transaction.Effect   (TxEffs, runTx, newTransactionContext)
import           Tendermint.SDK.BaseApp.Transaction.Modifier
import           Tendermint.SDK.BaseApp.Transaction.Types
import           Tendermint.SDK.Codec                        (HasCodec (..))
import           Tendermint.SDK.Types.Effects                ((:&))
import           Tendermint.SDK.BaseApp.Store (ReadStore, WriteStore)
import           Tendermint.SDK.Types.Message                (HasMessageType (..),
                                                              Msg (..))
import           Tendermint.SDK.Types.TxResult               (TxResult)
import Data.Singletons (Sing, fromSing)

--------------------------------------------------------------------------------

class HasTxRouter layout r (c :: RouteContext) where
  type RouteTx layout r c :: *
  routeTx
        :: Proxy layout
        -> Proxy r
        -> Sing c
        -> R.Delayed (Sem r) env (RoutingTx ByteString) (RouteTx layout r c)
        -> R.Router env r (RoutingTx ByteString) TxResult

instance (HasTxRouter a r c, HasTxRouter b r c) => HasTxRouter (a :<|> b) r c where
  type RouteTx (a :<|> b) r c = RouteTx a r c :<|> RouteTx b r c

  routeTx _ pr sc server =
    R.choice (routeTx pa pr sc ((\ (a :<|> _) -> a) <$> server))
             (routeTx pb pr sc ((\ (_ :<|> b) -> b) <$> server))
    where pa = Proxy :: Proxy a
          pb = Proxy :: Proxy b

instance (HasTxRouter sublayout r c, KnownSymbol path) => HasTxRouter (path :> sublayout) r c where

  type RouteTx (path :> sublayout) r c = RouteTx sublayout r c

  routeTx _ pr sc subserver =
    R.pathRouter (cs (symbolVal proxyPath)) (routeTx (Proxy :: Proxy sublayout) pr sc subserver)
    where proxyPath = Proxy :: Proxy path

methodRouter
  :: HasCodec a
  => Members [Embed IO, ReadStore, WriteStore] r
  => Sing (c :: RouteContext)
  -> R.Delayed (Sem r) env (RoutingTx msg) (Sem (TxEffs :& r) a)
  -> R.Router env r (RoutingTx msg) TxResult
methodRouter sc action = 
  let route' env tx = do
        ctx <- liftIO $ newTransactionContext (fromSing sc) tx
        let action' = runTx ctx <$> action
        R.runAction action' env tx R.Route
  in R.leafRouter route'

instance ( HasMessageType msg, HasCodec msg, HasCodec (OnCheckReturn c oc a), Members [ReadStore, WriteStore, Embed IO] r) => HasTxRouter (TypedMessage msg :~> Return' oc a) r c where

  type RouteTx (TypedMessage msg :~> Return' oc a) r c = RoutingTx msg -> Sem (TxEffs :& r) (OnCheckReturn c oc a)

  routeTx _ _ sc subserver =
    let f (RoutingTx tx@Tx{txMsg}) =
          if msgType txMsg == mt
            then case decode $ msgData txMsg of
              Left e -> R.delayedFail $
                R.InvalidRequest ("Failed to parse message of type " <> mt <> ": " <> e <> ".")
              Right a -> pure . RoutingTx $ tx {txMsg = txMsg {msgData = a}}
            else R.delayedFail R.PathNotFound
    in methodRouter sc $
         R.addBody subserver $ R.withRequest f
      where mt = messageType (Proxy :: Proxy msg)

emptyTxServer :: RouteTx EmptyTxServer r c
emptyTxServer = EmptyTxServer

instance HasTxRouter EmptyTxServer r c where
  type RouteTx EmptyTxServer r c = EmptyTxServer
  routeTx _ _ _ _ = R.StaticRouter mempty mempty
